/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.cloudant

import java.util.Date

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.model.StatusCodes
import akka.pattern.after
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.models._
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MigrationDBMethods(client: SimpleCloudantClient) extends MigrationConstant with StrictLogging {

  private def convertToMigrationResources(dbResources: JsValue): MigrationResources = {
    Try {
      logger.trace(s"start to convert migration resources ${logPrint(dbResources)}")
      val migrations = dbResources.convertTo[Seq[MigrationDoc]]
      val results = migrations.map { migration =>
        convertToMigrationResource(migration.toJson)
      }
      MigrationResources(results)
    } match {
      case Success(resources) => resources
      case Failure(ex) =>
        ExceptionLogger.log(Some("An error occurred when convert db info to migration resources"), ex, None, callStack = true)
        throw ServiceException(StatusCodes.InternalServerError, FailedConvertMigrationResources(ex))
    }
  }

  private def convertToMigrationResource(dbResource: JsValue): MigrationResource = {
    Try {
      logger.trace(s"start to convert migration resource ${logPrint(dbResource)}")
      val migrationDoc = dbResource.convertTo[MigrationDoc]
      MigrationResource(
        migrationDoc.id.getOrElse(""),
        migrationDoc.status,
        migrationDoc.results,
        migrationDoc.console,
        migrationDoc.mapping,
        migrationDoc.oldInstance,
        migrationDoc.spaceId,
        migrationDoc.projectId,
        migrationDoc.createdAt,
        migrationDoc.modifiedAt
      )
    } match {
      case Success(resources) => resources
      case Failure(ex) =>
        ExceptionLogger.log(Some("An error occurred when convert db info to migration resource"), ex, None, callStack = true)
        throw ServiceException(StatusCodes.InternalServerError, FailedConvertMigrationResources(ex))
    }
  }

  private def convertToMigrationRequest(request: JsValue): MigrationRequest = {
    Try {
      logger.trace(s"start to convert migration request ${logPrint(request)}")
      request.convertTo[MigrationRequest]
    } match {
      case Success(resources) => resources
      case Failure(ex) =>
        ExceptionLogger.log(Some("An error occurred when convert json payload to migration request"), ex, None, callStack = true)
        throw ServiceException(StatusCodes.BadRequest, FailedConvertMigrationResources(ex))
    }
  }

  private def getViewNameAndId(spaceId: Option[String],
                               projectId: Option[String]): (String, String) = {
    if (spaceId.isDefined) {
      (LIST_JOB_BY_SPACE_ID_VIEW, spaceId.get)
    } else if (projectId.isDefined) {
      (LIST_JOB_BY_PROJECT_ID_VIEW, projectId.get)
    } else {
      throw ServiceException(StatusCodes.BadRequest, MissingOneOfQueryParametersMessage("space_id", "project_id"))
    }
  }

  def getAllMigrationJobs(identity: Identity,
                          spaceId: Option[String],
                          projectId: Option[String])
                         (implicit ec: ExecutionContext): Future[MigrationResources] = {
    val (viewName, id) = getViewNameAndId(spaceId, projectId)
    for {
      json <- client.view(viewName, Some(List(id)))
      migrationResources <- Future.fromTry {
        Try {
          convertToMigrationResources(json)
        }
      }
    } yield {
      migrationResources
    }
  }

  def createMigrationJob(identity: Identity,
                         jsonEntity: JsValue)
                        (implicit ec: ExecutionContext): Future[MigrationResource] = {
    for {
      dbPayload <- Future.fromTry {
        Try {
          val request = convertToMigrationRequest(jsonEntity)
          MigrationDoc(userId = identity.subject.id,
            status = Pending,
            createdAt = new Date().getTime,
            modifiedAt = new Date().getTime,
            oldInstance = request.oldInstance,
            results = Some(MigrationResults()),
            console = Some(Seq.empty),
            mapping = request.mapping,
            spaceId = request.spaceId,
            projectId = request.projectId,
            experimentIds = request.experimentIds,
            functionIds = request.functionIds,
            modelIds = request.modelIds,
            pipelineIds = request.pipelineIds,
            skipMigratedAssets = request.skipMigratedAssets)
        }
      }
      json <- client.save(dbPayload.toJson)
    } yield {
      convertToMigrationResource(json)
    }
  }

  private def checkSpaceOrProjectId(doc: MigrationDoc,
                                    spaceId: Option[String],
                                    projectId: Option[String]): Boolean = {
    if (spaceId.isDefined) {
      spaceId == doc.spaceId
    } else if (projectId.isDefined) {
      projectId == doc.projectId
    } else {
      throw ServiceException(StatusCodes.BadRequest, MissingOneOfQueryParametersMessage("space_id", "project_id"))
    }
  }

  def getMigrationJob(migrationId: String,
                      identity: Identity,
                      spaceId: Option[String],
                      projectId: Option[String])
                     (implicit ec: ExecutionContext): Future[MigrationResource] = {
    for {
      json <- client.find(migrationId)
    } yield {
      val doc = json.convertTo[MigrationDoc]
      if (!checkSpaceOrProjectId(doc, spaceId, projectId)) {
        throw ServiceException(StatusCodes.Forbidden, JobAccessForbidden(migrationId))
      }
      convertToMigrationResource(json)
    }
  }

  def deleteMigrationJobPreCheck(migrationId: String,
                                 identity: Identity,
                                 spaceId: Option[String],
                                 projectId: Option[String])
                                (implicit ec: ExecutionContext): Future[Unit] = {
    for {
      migrationDoc <- getMigrationDoc(migrationId)
      _ = if (!checkSpaceOrProjectId(migrationDoc, spaceId, projectId)) {
        throw ServiceException(StatusCodes.Forbidden, JobAccessForbidden(migrationId))
      }
    } yield {}
  }

  def cancelMigrationJobPreCheck(migrationId: String,
                                 identity: Identity,
                                 spaceId: Option[String],
                                 projectId: Option[String])
                                (implicit ec: ExecutionContext): Future[Unit] = {
    for {
      migrationDoc <- getMigrationDoc(migrationId)
      _ = if (!checkSpaceOrProjectId(migrationDoc, spaceId, projectId)) {
        throw ServiceException(StatusCodes.Forbidden, JobAccessForbidden(migrationId))
      }
      _ = if (migrationDoc.status.isTerminated) {
        throw ServiceException(StatusCodes.BadRequest, JobAlreadyTerminated(migrationId))
      }
    } yield {}
  }

  def deleteMigrationJob(migrationId: String)
                        (implicit ec: ExecutionContext): Future[Unit] = {
    for {
      latestDoc <- getMigrationDoc(migrationId)
      _ <- client.remove(latestDoc.toJson)
    } yield {}
  }

  def cancelMigrationJob(migrationId: String)
                        (implicit ec: ExecutionContext, system: ActorSystem): Future[Unit] = {

    def retry[T](op: => T, delay: FiniteDuration, retries: Int)(implicit ec: ExecutionContext): Future[T] = {
      val s: Scheduler = system.scheduler
      Future(op) recoverWith { case _ if retries > 0 => after(delay, s)(retry(op, delay, retries - 1)) }
    }

    retry(
      for {
        latestDoc <- getMigrationDoc(migrationId)
        _ <- updateStatus(Canceled, latestDoc)
      } yield {}, 15.second, 5
    )
  }


  def updateStatus(status: MigrationStatus,
                   migration: MigrationDoc)
                  (implicit ec: ExecutionContext): Future[MigrationDoc] = {
    updateMigrationDoc(migration.copy(status = status))
  }

  def updateMigrationDoc(migration: MigrationDoc)
                        (implicit ec: ExecutionContext): Future[MigrationDoc] = {
    // update the status when it is not terminated
    val jobId = migration.id.get
    for {
      latestDoc <- getMigrationDoc(jobId)
      // here we only use the latestDoc to check the status
      json <- if (latestDoc.status.isTerminated) {
        // skip update
        Future.failed(ServiceException(StatusCodes.BadRequest, JobAlreadyTerminated(jobId)))
      } else {

        val doc = migration.copy(modifiedAt = new Date().getTime)
        client.update(doc.toJson)
      }
    } yield {
      json.convertTo[MigrationDoc]
    }
  }

  def getMigrationDoc(migrationId: String)
                     (implicit ec: ExecutionContext): Future[MigrationDoc] = {
    logger.debug(s"start get migration Doc for id $migrationId")
    for {
      json <- client.find(migrationId)
    } yield {
      json.convertTo[MigrationDoc]
    }
  }

  def getAllMigrationJobsByStatus(Status: MigrationStatus)
                                 (implicit ec: ExecutionContext): Future[MigrationResources] = {
    for {
      json <- client.view(LIST_JOB_BY_STATUS_VIEW, Some(List(Status.name)))
    } yield {
      convertToMigrationResources(json)
    }
  }

  def getAllRunningMigrationJobs()(implicit ec: ExecutionContext): Future[MigrationResources] = {
    for {
      json <- client.view(GET_RUNNING_JOB_VIEW, None)
    } yield {
      convertToMigrationResources(json)
    }
  }

  def getAllNonTerminatedMigrationJobs()(implicit ec: ExecutionContext): Future[MigrationResources] = {
    for {
      json <- client.view(GET_NON_TERMINATED_JOB_VIEW, None)
    } yield {
      convertToMigrationResources(json)
    }
  }

  def getNextJobId(implicit ec: ExecutionContext): Future[Option[String]] = {
    for {
      json <- client.view(GET_NEXT_JOB_VIEW, None)
    } yield {
      val rows = json.convertTo[Seq[NextJobId]]
      if (rows.nonEmpty) {
        Some(rows.head.id)
      } else None
    }
  }
}
