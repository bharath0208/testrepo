/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.job

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.stream.{KillSwitches, SharedKillSwitch}
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.models.{Failed, Initializing, Pending}
import com.ibm.ml.repository.v4.migration.service.MigrationServiceContext
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

case class JobsManager(msc: MigrationServiceContext)(implicit system: ActorSystem) extends StrictLogging {

  private implicit val ec: ExecutionContext = system.dispatcher

  val monitorMap: mutable.Map[String, SharedKillSwitch] = mutable.Map()
  val maxJobNumber: Int = Try(config.getInt("service.migration-job.maximum-jobs")).getOrElse(10)
  val launchJobIntervalMinutes: Int = Try(config.getInt("service.migration-job.launch-job-interval-minutes")).getOrElse(1)

  case class SkipLaunchJob() extends Exception("skip launch next job")

  logger.info(s"run job launcher every $launchJobIntervalMinutes mins")
  system.scheduler.scheduleWithFixedDelay(10.seconds, launchJobIntervalMinutes.minutes) {
    () => {
      launchNextJob()
    }
  }(system.dispatcher)


  def launchNextJob(): Future[Unit] = {

    (for {
      // get all the running job number
      jobs <- msc.dbMethods.getAllRunningMigrationJobs()
      _ = if (jobs.resources.length > maxJobNumber) {
        logger.info(s"Skip launch next job since running jobs number ${jobs.resources.length} is more then max jobs number $maxJobNumber")
        throw SkipLaunchJob()
      }
      _ = logger.debug(s"Current running jobs number is ${jobs.resources.length}")
      // get next job id
      nextJobIdOpt <- msc.dbMethods.getNextJobId
      nextJobId = nextJobIdOpt.getOrElse({
        logger.info(s"Skip launch next job since we don't have any pending job")
        throw SkipLaunchJob()
      })
      _ = logger.debug(s"Next job id is $nextJobId")
      // try to update the status of the job
      migrationDoc <- msc.dbMethods.getMigrationDoc(nextJobId)
      _ = {
        logger.debug(s"Next job $nextJobId status is ${migrationDoc.status}")
        if (migrationDoc.status != Pending) {
          logger.info(s"Skip launch next job since the job already started")
          throw SkipLaunchJob()
        }
      }

      updateDoc <- msc.dbMethods.updateStatus(Initializing, migrationDoc).recoverWith {
        case se: ServiceException =>
          if (se.status == StatusCodes.Conflict.intValue) {
            logger.info("Skip the launch the job due to cloudant doc conflict")
            throw SkipLaunchJob()
          } else throw se
        case e: Throwable => throw e
      }
      _ = logger.debug(s"Next job $nextJobId status update to ${updateDoc.status}")
      base64EncodedDetails = base64Encode(updateDoc.toJson.compactPrint).getOrElse("")
      // start kube job, use updateDoc.id.get  instead of the nextJobId to make sure we run the operation in series
      jobName <- msc.kubeClient.createJob(
        updateDoc.id.get,
        base64EncodedDetails
      ).recoverWith {
        case e: Throwable =>
          // mark the job as failed since the createJob failed
          val msg = s"Failed to create kube job due to ${e.getMessage}"
          msc.dbMethods.updateStatus(Failed, updateDoc.addMessageToConsole(msg))
          throw e
      }
    } yield {
      // start to monitor the kube job
      val sharedKillSwitch = KillSwitches.shared(nextJobId)
      kubeJobMonitoring(jobName, nextJobId, sharedKillSwitch)
      // add kill switch to map
      monitorMap.addOne((nextJobId, sharedKillSwitch))
      logger.info(s"Migration job $nextJobId launched")
    }) recoverWith {
      case _: SkipLaunchJob =>
        logger.info("Skip job launch")
        Future.successful(())
      case e: Throwable =>
        ExceptionLogger.log(Some("Failed to launch next job due to the error"), e, None, callStack = true)
        throw e
    }
  }

  def deleteJob(jobId: String): Future[Unit] = {
    // try to check if the shared kill switch is in this pod, if not it will kill it self in an other pod
    // this is just for the double safe to prevent the thread leak
    monitorMap.get(jobId).foreach { killSwitch =>
      killSwitch.shutdown()
      monitorMap.remove(jobId)
    }
    (for {
      // delete the job
      _ <- msc.kubeClient.deleteJobByName(msc.kubeClient.getJobName(jobId))
    } yield {}) recoverWith {
      case e: Throwable =>
        logger.info(s"Delete job failed due the error $e, but we ignore will ignore it if it failed")
        Future.successful(())
    }
  }


  private def kubeJobMonitoring(jobName: String, jobId: String, sharedKillSwitch: SharedKillSwitch): Future[Done] = {
    logger.debug(s"Starting to monitor Kube Job status for kube job $jobName")


    msc.kubeClient.kubeClient.watchKubeJob(jobName, sharedKillSwitch, jobInfo => {
      jobInfo.status match {
        case Some(status) =>
          logger.info(s"Kube job $jobName status", status)
          // kube api is using this weird way to determine a job is succeeded or failed
          // to read more https://github.com/kubernetes/kubernetes/issues/68712#issuecomment-514008330
          val jobFailed = status.conditions.count(condition =>
            condition.`type` == "Failed" && condition.status == "True") > 0
          val jobSucceeded = status.conditions.count(condition =>
            condition.`type` == "Complete" && condition.status == "True") > 0
          logger.debug(s"Kube Job $jobName status Failed: $jobFailed Succeeded: $jobSucceeded.")
          if (jobFailed || jobSucceeded) {
            // if job Done stop the monitor first
            sharedKillSwitch.shutdown()
            logger.debug(s"Kube Job $jobName done, shutdown kube monitor")
            // remove the sharedSwitch from map
            monitorMap.remove(jobId)
            // check the latest status for a job, if the status is terminated then we dont need to check
            // delay 1 min to make sure all the status update is done
            system.scheduler.scheduleOnce(1.minute) {
              for {
                doc <- msc.dbMethods.getMigrationDoc(jobId)
                _ <- if (!doc.status.isTerminated) {
                  if (jobSucceeded) {
                    val msg = "Migration job finished succeeded without update the status to Completed"
                    msc.dbMethods.updateStatus(Failed, doc.addMessageToConsole(msg))
                  } else {
                    logger.info(s"The migration job $jobId failed: ${logPrint(doc.toJson)}")
                    val msg = "Unknown Migration job failure"
                    msc.dbMethods.updateStatus(Failed, doc.addMessageToConsole(msg))
                  }
                } else Future.successful(())
                // delete the job after the job is finished
                _ <- deleteJob(jobId)

              } yield {}
            }
            launchNextJob()
          }
        case None =>
      }
    })
  }

}
