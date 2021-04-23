/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.endpoints

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat.migrationRequestFormat
import com.ibm.ml.repository.v4.migration.models.{MigrationRequest, MigrationResource, MigrationResources}
import com.ibm.ml.repository.v4.migration.service.MigrationServiceContext
import com.ibm.ml.repository.v4.migration.utils.MigrationJobRejected
import com.ibm.ml.repository.v4.utils.ServiceException
import com.ibm.ml.repository.v4.utils.logging.{ExceptionLogger, reqId}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object MigrationEndpoints extends StrictLogging {
  private def checkNewMigrationJob(identity: Identity,
                                   msc: MigrationServiceContext,
                                   jsonEntity: JsValue)
                                  (implicit ec: ExecutionContext): Future[Unit] = {
    val job = Try(jsonEntity.convertTo[MigrationRequest])

    if (job.isFailure) {
      // don't do any checks and let the normal error handling catch this
      Future.successful(())
    } else {
      //
      validateJob(identity, job.get)
      // we might want a view for this but as we are not completely sure
      // what tests should be done here we can do it like this for now
      for {
        // only look at jobs that are not terminated
        jobs <- msc.dbMethods.getAllNonTerminatedMigrationJobs() recover {
          case t: Throwable =>
            ExceptionLogger.log(Some(s"Not able to check new migration job due to error when fetching jobs: ${t.getMessage}"), t, identity.requestId)
            // we don't fail for this - we just don't check?
            MigrationResources()
        }
      } yield {
        // see if we already have a job for this old_instance/container pair
        for (running <- jobs.resources) {
          if ((job.get.oldInstance.instanceId == running.oldInstance.instanceId) &&
            (job.get.getContainer == running.getContainer)) {
            // for now we don't allow this
            val msg: String = s"Rejected migration job because there is already a job '${running.migrationId}' in state '${running.status.name}' for this instance id '${job.get.oldInstance.instanceId}' and ${job.get.getContainer.getOrElse("")}"
            reqId(identity.requestId)(() => logger.debug(msg))
            throw ServiceException(StatusCodes.BadRequest, MigrationJobRejected(msg))
          }
        }
      }
    }
  }

  def getAllMigrationJobs(identity: Identity,
                          msc: MigrationServiceContext,
                          spaceId: Option[String],
                          projectId: Option[String])
                         (implicit ec: ExecutionContext): Future[MigrationResources] = {
    msc.dbMethods.getAllMigrationJobs(identity, spaceId, projectId)
  }

  def createMigrationJob(identity: Identity,
                         msc: MigrationServiceContext,
                         jsonEntity: JsValue)
                        (implicit ec: ExecutionContext): Future[MigrationResource] = {
    for {
      _ <- checkNewMigrationJob(identity, msc, jsonEntity)
      migrationResource <- msc.dbMethods.createMigrationJob(identity, jsonEntity)
    } yield {
      msc.jobsManager.launchNextJob()
      migrationResource
    }
  }

  def getMigrationJob(identity: Identity,
                      msc: MigrationServiceContext,
                      migrationId: String,
                      spaceId: Option[String],
                      projectId: Option[String])
                     (implicit ec: ExecutionContext): Future[MigrationResource] = {
    msc.dbMethods.getMigrationJob(migrationId, identity, spaceId, projectId)
  }

  def deleteMigrationJob(identity: Identity,
                         msc: MigrationServiceContext,
                         migrationId: String,
                         hardDelete: Boolean = false,
                         spaceId: Option[String],
                         projectId: Option[String])
                        (implicit ec: ExecutionContext,
                         system: ActorSystem): Future[Unit] = {
    if (hardDelete) {
      for {
        _ <- msc.dbMethods.deleteMigrationJobPreCheck(migrationId, identity, spaceId, projectId)
        _ <- msc.jobsManager.deleteJob(migrationId)
        _ <- msc.dbMethods.deleteMigrationJob(migrationId)
      } yield {}
    } else {
      for {
        _ <- msc.dbMethods.cancelMigrationJobPreCheck(migrationId, identity, spaceId, projectId)
        _ <- msc.dbMethods.cancelMigrationJob(migrationId)
        _ <- msc.jobsManager.deleteJob(migrationId)
      } yield {}
    }
  }

  private val ASSETS_ALL = "all"

  private def validateJob(identity: Identity, job: MigrationRequest): Unit = {
    def rejectJob(assetType: String, id: String): Unit = {
      val msg: String = s"Invalid $assetType id : '$id'"
      reqId(identity.requestId)(() => logger.debug(msg))
      throw ServiceException(StatusCodes.BadRequest, MigrationJobRejected(msg))
    }

    def isValidUUID(id: String): Boolean = {
      id.equalsIgnoreCase(ASSETS_ALL) || Try(UUID.fromString(id)).filter(_.toString.equals(id)).isSuccess
    }

    job.experimentIds.map(_.map(id => if (!isValidUUID(id)) rejectJob("experiment", id)))
    job.pipelineIds.map(_.map(id => if (!isValidUUID(id)) rejectJob("pipeline", id)))
    job.functionIds.map(_.map(id => if (!isValidUUID(id)) rejectJob("function", id)))
    job.modelIds.map(_.map(id => if (!isValidUUID(id)) rejectJob("model", id)))
  }
}
