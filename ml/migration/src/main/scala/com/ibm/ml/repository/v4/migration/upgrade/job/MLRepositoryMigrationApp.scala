/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.job

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.analytics.wml.utils.security.ConfigDecrypter
import com.ibm.ml.repository.v4.migration.upgrade.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.upgrade.models._
import com.ibm.ml.repository.v4.migration.utils.{MLRepositoryMigrationAppFailed, MigrationConstant}
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.{Logger, StrictLogging}
import org.slf4j.LoggerFactory
import spray.json._

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MLRepositoryMigrationApp(args: Array[String]) extends ConfigDecrypter with MigrationConstant with StrictLogging {
  private val specificationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationApp]("Specification-Version")
  private val implementationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationApp]("Implementation-Version")
  logger.info(s"Initialize ML Repository Migration App Context with App Implementation Version: $implementationVersion and Specification Version: $specificationVersion")

  // we create an instance rather than using inheritance so that this
  // class gets created before the context
  private class Ctxt() extends MigrationAppContext

  implicit val ctxt: MigrationAppContext = {
    Try {
      new Ctxt()
    } match {
      case Success(context) =>
        context
      case Failure(exception) =>
        ExceptionLogger.log(Some("Failed to initialize ml-repository migration App context"), exception, None, callStack = true)
        throw utils.ServiceException(
          StatusCodes.InternalServerError,
          MLRepositoryMigrationAppFailed()
        )
    }
  }

  def startJob(identity: Identity, doc: MigrationDoc): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher

    val migrationId = doc.id.getOrElse(
      throw ServiceException(StatusCodes.InternalServerError, MissingRequiredEnvironmentVariable(MIGRATION_JOB_ID))
    )

    (for {
      softwareSpecs <- ctxt.environmentsClient.getSoftwareSpecsResources(identity,
        None,
        container = Some(getContainer(doc.newSpaceId, doc.newProjectId)))
      hardwareSpecs <- ctxt.environmentsClient.getHardwareSpecsResources(identity,
        None,
        container = Some(getContainer(doc.newSpaceId, doc.newProjectId)))
      _ <- MigrationJob(ctxt, doc, softwareSpecs, hardwareSpecs)(identity).start()
    } yield {}) recoverWith {
      case t: Throwable =>
        val msg = s"Migration job failed due to ${t.getMessage}"
        (for {
          doc <- ctxt.dbMethods.getMigrationDoc(migrationId)
          _ <- ctxt.dbMethods.updateStatus(Failed, doc.addMessageToConsole(msg))
        } yield {}).recover(_ => Future.successful(()))
    }
  }

  def startJobs(identity: Identity, jobs: Vector[MigrationDoc]): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher

    seqFutures(jobs) { doc =>
      logger.info(s"Starting migration job ${doc.id.getOrElse("")}")
      startJob(identity, doc)
    }.map(_ => logger.info("finished all the migration sub job"))
  }

  def start(): Future[Vector[MigrationDoc]] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher
    sys.addShutdownHook(system.terminate())
    for {
      identity <- ctxt.getIdentity
      jobs <- MigrationJobsGenerator(ctxt)(identity).getJobs
      _ <- startJobs(identity, jobs)
      updatedJobs <- ctxt.dbMethods.getAllMigrationDocs
    } yield {
      logger.info("All the migration jobs in the db:")
      logger.info("=================================")
      logger.info(logPrint(updatedJobs.toJson))
      logger.info("=================================")
      val latestJobs = updatedJobs.filter { newDoc =>
        jobs.exists(_.id == newDoc.id)
      }
      logger.info("Migration jobs for this run:")
      logger.info("=================================")
      logger.info(logPrint(latestJobs.toJson))
      logger.info("=================================")
      latestJobs
    }
  }
}

object MLRepositoryMigrationJobApp extends App with StrictLogging {
  Try {
    Await.result(MLRepositoryMigrationApp(args).start(), 24.hours)
  } match {
    case Success(jobs) =>
      val failed: mutable.HashMap[String, (MigrationFailedResult, Container)] = mutable.HashMap()
      val successful: mutable.HashMap[String, (MigrationSuccessfulResult, Container)] = mutable.HashMap()

      for (job <- jobs) {
        val container: Container = if (job.oldSpaceId.isDefined) Space(job.oldSpaceId.get) else Project(job.oldProjectId.get)
        job.results match {
          case Some(results) =>
            // add any new failed results
            for (failedResult <- results.failed) {
              failed.put(failedResult.oldId, (failedResult, container))
            }
            // remove any successful results
            for (successfulResult <- results.successful) {
              failed.remove(successfulResult.oldId)
              successful.put(successfulResult.oldId, (successfulResult, container))
            }
          case None =>
        }
      }
      val fc: Int = failed.size
      val sc: Int = successful.size

      // separate logger because this summary should go to the cpd installer log (see with Shashank)
      val failedLogger: Logger = Logger(LoggerFactory.getLogger("user.migration.failed.summary"))
      val successfulLogger: Logger = Logger(LoggerFactory.getLogger("user.migration.successful.summary"))

      failedLogger.info("WML: Repository Migration Summary (start)")
      failedLogger.info("WML: ====================================")
      if (fc > 0) {
        failedLogger.info(s"WML:  Found $fc old asset${if (fc > 1) "s" else ""} that failed to migrate:")
        for (f <- failed) {
          failedLogger.info(s"WML:    ${f._2._1.oldAssetType} ${f._1} in ${f._2._2} failed to migrate: ${f._2._1.reason}")
        }
      } else {
        failedLogger.info(s"WML:  All assets were successfully migrated")
      }
      failedLogger.info("WML: Repository Migration Summary (end)")
      /////////////////////////////////////////////////////////////////////////////
      successfulLogger.info("WML: Repository Migration Summary (start)")
      successfulLogger.info("WML: ====================================")
      if(sc > 0) {
        successfulLogger.info(s"WML:  $sc asset${if (sc > 1) "s  were" else " was"} successfully migrated:")
        for (s <- successful) {
          successfulLogger.info(s"WML:    ${s._2._1.oldAssetType} ${s._1} in ${s._2._2} now has id ${s._2._1.newId}")
        }
      }
      successfulLogger.info("WML: Repository Migration Summary (end)")

      logger.info(s"Job terminated normally with $fc failed assets")
      System.exit(fc)
    case Failure(exception) =>
      ExceptionLogger.log(Some("Job terminated with an exception (status -1)"), exception, None, callStack = true)
      System.exit(-1)
  }
}
