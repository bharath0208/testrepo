/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.route

import java.util.Date

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.api.v4.common.Metadata
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.job.MigrationJob
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.models.{MigrationSuccessfulResult, _}
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.ibm.ml.repository.v4.utils.{FailedToConvertToRequest, FailedToCreateNewAsset, FailedToGetAsset, FailedToGetDependencies, FailedToMigrateAsset, FailedToMigrateDependencies, ServiceException, logPrint}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

abstract class AbstractMigrationRoute[OldResource, NewRequest](mj: MigrationJob)(implicit system: ActorSystem,
                                                                                 identity: Identity,
                                                                                 resourceJF: RootJsonFormat[OldResource],
                                                                                 requestJF: RootJsonFormat[NewRequest]) extends StrictLogging with MigrationConstant {
  implicit val ec: ExecutionContext = system.dispatcher

  def oldAssetType: String

  def newAssetType: String

  def createMigratedFrom(oldAssetId: String): MigratedFrom = {
    MigratedFrom(
      instanceId = mj.migrationDoc.oldInstance.instanceId,
      assetType = oldAssetType,
      assetId = oldAssetId
    )
  }

  def addMigratedFrom(custom: Option[JsObject], oldAssetId: String): Option[JsObject] = {
    val MIGRATED_FROM: String = "migrated_from"

    Try(createMigratedFrom(oldAssetId).toJson) match {
      case Success(from) =>
        custom match {
          case Some(jsObject) =>
            Some(JsObject(fields = jsObject.fields ++ Map(MIGRATED_FROM -> from)))
          case None =>
            Some(JsObject(MIGRATED_FROM -> from))
        }
      case Failure(exception) =>
        logger.warn(s"Failed to add '$MIGRATED_FROM' due to json error: ${exception.getMessage}")
        custom
    }
  }

  private def isMigrated(oldId: String, doc: MigrationDoc): Boolean = {
    doc.results.foreach { r =>
      r.successful.foreach { s =>
        if (s.oldId.contains(oldId)) return true
      }
      r.failed.foreach { s =>
        if (s.oldId.contains(oldId)) return true
      }
      r.skipped.foreach { skipped =>
        skipped.foreach { skip =>
          if (skip.oldId.contains(oldId)) return true
        }
      }
    }
    doc.mapping.foreach { map =>
      if (map.contains(oldId)) return true
    }
    false
  }

  private def shouldSkip(oldId: String, migratedAssets: Seq[MigrationSuccessfulResult], doc: MigrationDoc): Future[Option[MigrationSuccessfulResult]] = {
    if (doc.shouldSkipMigratedAssets) {
      migratedAssets.find(_.oldId == oldId).map { result =>
        for {
          exist <- checkNewAssetExist(result.newId, doc)
        } yield {
          if (exist) Some(result) else None
        }
      }.getOrElse(Future.successful(None))
    } else {
      Future.successful(None)
    }
  }

  protected def getNewAsset(newId: String, doc: MigrationDoc): Future[Any]

  private def checkNewAssetExist(newId: String, doc: MigrationDoc): Future[Boolean] = {
    (for {
      _ <- getNewAsset(newId, doc)
    } yield {
      true
    }).recoverWith {
      t: Throwable =>
        logger.debug(s"Failed to get new $newAssetType '$newId' for skip check")
        Future.successful(false)
    }
  }


  def migrate(oldId: String,
              migratedAssets: Seq[MigrationSuccessfulResult],
              doc: MigrationDoc): Future[MigrationDoc] = {
    //step 0 check we need to migrate this asset or not
    if (isMigrated(oldId, doc)) {
      // if already migrated skip
      logger.info(s"skip to migrate the asset $oldId since it already migrated")
      Future.successful(doc)
    } else {
      (for {
        // step 0 test we should skip to migrate or not
        skipResult <- shouldSkip(oldId, migratedAssets, doc)
        newDoc <- {
          if (skipResult.isDefined) {
            updateMigrationSkipResult(skipResult.get, doc)
          } else {
            migrateAsset(oldId, migratedAssets, doc)
          }
        }
      } yield {
        newDoc
      }) recoverWith {
        case e: Throwable =>
          ExceptionLogger.log(Some(s"Failed to migrate the $oldAssetType asset '$oldId'"), e, identity.requestId)
          val se = ServiceException(StatusCodes.BadRequest, FailedToMigrateAsset(oldId, oldAssetType, e.getMessage))
          handleMigrationFailure(oldId, None, doc, se)
      }
    }
  }


  private def migrateAsset(oldId: String, migratedAssets: Seq[MigrationSuccessfulResult], doc: MigrationDoc): Future[MigrationDoc] = {
    for {
      // step 1 get old resources
      (oldResource, newId, consoleMsgs) <- getOldAsset(oldId)
      updatedDoc = doc.addMessagesToConsole(consoleMsgs)
      newDoc <- {
        if (newId.isDefined) {
          val successfulResult = getSuccessfulResult(oldId, newId.get, oldResource)
          updateMigrationSkipResult(successfulResult, updatedDoc)
        } else {
          migrateFromOldResource(oldId, oldResource, migratedAssets, updatedDoc)
        }
      }.recoverWith {
        case e: Throwable =>
          val se = ServiceException(StatusCodes.BadRequest, FailedToMigrateAsset(oldId, oldAssetType, e.getMessage))
          // here we added some oldResource info to error
          handleMigrationFailure(oldId, Some(oldResource), updatedDoc, se)
      }
    } yield {
      newDoc
    }
  }

  private def migrateFromOldResource(oldId: String, oldResource: OldResource, migratedAssets: Seq[MigrationSuccessfulResult], doc: MigrationDoc): Future[MigrationDoc] = {
    for {
      // step 2 get the dependencies we need to migrate first
      // in this function we need to check whether the dependency already got migrated
      dependencies <- getDependencies(oldResource, doc)
      // step 3 migrate dependencies
      docWithDependencies <- migrateDependencies(dependencies, migratedAssets, doc)
      // step 4 convert to request payload
      (request, consoleMessages) <- convertToRequest(oldResource, dependencies, docWithDependencies)
      // step 5 make request to create new asset
      // this step should include the content upload
      newId <- createNewAsset(request, oldResource, docWithDependencies)
      // step 6 update the cloudant with new id
      newDoc <- updateMigrationResult(oldId, newId, oldResource, docWithDependencies, consoleMessages)
    } yield {
      logger.info(s"Migration for $oldAssetType asset $oldId is finished, create a new $newAssetType asset $newId")
      newDoc
    }
  }

  private def getNameAndLastModified(oldResource: Option[OldResource]): (Option[String], Option[Date]) = {
    import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat.metaDataFormat
    val jsonOpt = oldResource.map(_.toJson)
    val metadata = jsonOpt.flatMap(_.asJsObject.fields.get("metadata")).map(_.convertTo[Metadata])
    val name = metadata.flatMap(_.name) match {
      case Some(value) => Some(value)
      case None =>
        val entity = jsonOpt.flatMap(_.asJsObject.fields.get("entity"))
        entity.flatMap(_.asJsObject.fields.get("name").map(_.asInstanceOf[JsString].value))
    }
    val lastModified = metadata.flatMap(_.modifiedAt)
    (name, lastModified)
  }


  private def handleMigrationFailure(oldId: String,
                                     oldResource: Option[OldResource],
                                     doc: MigrationDoc,
                                     se: ServiceException): Future[MigrationDoc] = {

    val (name, lastModified) = getNameAndLastModified(oldResource)
    for {
      latestDoc <- mj.sc.dbMethods.getMigrationDoc(doc.id.get)
      results = latestDoc.results.getOrElse(MigrationResults())
      newResult = results.copy(failed = results.failed.appended(
        MigrationFailedResult(oldAssetType, oldId, name, se.message, lastModified)
      ))
      newDoc = latestDoc.copy(results = Some(newResult))
      resultDoc <- updateDocForMigration(newDoc)
    } yield {
      resultDoc
    }
  }

  private def getSuccessfulResult(oldId: String, newId: String, oldResource: OldResource): MigrationSuccessfulResult = {
    val (name, lastModified) = getNameAndLastModified(Some(oldResource))
    MigrationSuccessfulResult(oldAssetType, newAssetType, oldId, newId, name.getOrElse(""), lastModified)
  }

  private def updateMigrationResult(oldId: String, newId: String, oldResource: OldResource, doc: MigrationDoc, consoleMessages: Seq[String] = Seq.empty): Future[MigrationDoc] = {

    val successfulResult = getSuccessfulResult(oldId, newId, oldResource)
    val results = doc.results.getOrElse(MigrationResults())
    val newResult = results.copy(successful = results.successful.appended(successfulResult))
    val newDoc = doc.copy(results = Some(newResult)).addMessagesToConsole(consoleMessages)
    updateDocForMigration(newDoc)
  }

  private def updateMigrationSkipResult(result: MigrationSuccessfulResult, doc: MigrationDoc): Future[MigrationDoc] = {

    val newDoc = doc.addSkipped(result)
    updateDocForMigration(newDoc)
  }

  private def updateDocForMigration(newDoc: MigrationDoc): Future[MigrationDoc] = {
    (for {
      resultDoc <- mj.sc.dbMethods.updateMigrationDoc(newDoc)
    } yield {
      logger.trace(s"Cloudant doc update to ${logPrint(resultDoc.toJson)}")
      resultDoc
    }) recoverWith {
      case t: Throwable =>
        // if we cannot update doc anymore we need to terminate the job.
        // either we have a bug or the job status is set to terminated by api
        ExceptionLogger.log(Some(s"Failed to update the cloudant document, terminate the job"), t, identity.requestId)
        throw t
    }
  }


  private def getOldAsset(id: String): Future[(OldResource, Option[String], Seq[String])] = {

    (for {
      (resource, newId, consoleMsgs) <- getOldAssetAction(id)
    } yield {
      logger.trace(s"Asset $id resource: ${logPrint(resource.toJson)}")
      if (newId.isDefined) {
        logger.info(s"found new id $id, skip create new asset")
      }
      (resource, newId, consoleMsgs)
    }) recoverWith {
      case e: Throwable =>
        ExceptionLogger.log(Some(s"Failed to get the $oldAssetType asset $id"), e, identity.requestId)
        throw ServiceException(StatusCodes.BadRequest, FailedToGetAsset(id, oldAssetType, e.getMessage))
    }
  }

  protected def getOldAssetAction(id: String): Future[(OldResource, Option[String], Seq[String])]

  private def getDependencies(oldResource: OldResource,
                              doc: MigrationDoc): Future[MigrationIds] = {

    (for {
      dependencies <- getDependenciesAction(oldResource, doc)
    } yield {
      logger.info(s"Dependencies need to be migrate first: ${dependencies.toJson}")
      dependencies
    }) recoverWith {
      case e: Throwable =>
        ExceptionLogger.log(Some(s"Failed to resolve the dependencies asset: $oldResource"), e, identity.requestId)
        throw ServiceException(StatusCodes.BadRequest, FailedToGetDependencies(e.getMessage))
    }

  }

  protected def getDependenciesAction(oldResource: OldResource,
                                      doc: MigrationDoc): Future[MigrationIds]


  private def migrateDependencies(dependencies: MigrationIds,
                                  migratedAssets: Seq[MigrationSuccessfulResult],
                                  doc: MigrationDoc): Future[MigrationDoc] = {
    (for {
      doc <- mj.startMigrate(dependencies, migratedAssets, doc)
    } yield {
      logger.debug(s"doc rev after migrate dependencies: ${doc.rev}")
      doc
    }) recoverWith {
      case e: Throwable =>
        ExceptionLogger.log(Some(s"Failed to migrate dependencies"), e, identity.requestId)
        throw ServiceException(StatusCodes.BadRequest, FailedToMigrateDependencies(e.getMessage))
    }
  }


  private def convertToRequest(oldResource: OldResource,
                               dependencies: MigrationIds,
                               doc: MigrationDoc): Future[(NewRequest, Seq[String])] = {

    (for {
      (request, consoleMessages) <- convertToRequestAction(oldResource, dependencies, doc)
    } yield {
      logger.debug(s"new $newAssetType asset request payload : ${logPrint(request.toJson)}")
      (request, consoleMessages)
    }) recoverWith {
      case e: Throwable =>
        ExceptionLogger.log(Some(s"Failed to convert to the new schema"), e, identity.requestId)
        throw ServiceException(StatusCodes.BadRequest, FailedToConvertToRequest(e))
    }

  }

  protected def convertToRequestAction(oldResource: OldResource,
                                       dependencies: MigrationIds,
                                       doc: MigrationDoc): Future[(NewRequest, Seq[String])]

  private def createNewAsset(request: NewRequest,
                             oldResource: OldResource,
                             doc: MigrationDoc): Future[String] = {
    (for {
      id <- createNewAssetAction(request, oldResource, doc)
    } yield {
      logger.debug(s"new $newAssetType asset $id is created")
      id
    }) recoverWith {
      case e: Throwable =>
        ExceptionLogger.log(Some(s"Failed to create new v4 asset: $request"), e, identity.requestId)
        throw ServiceException(StatusCodes.BadRequest, FailedToCreateNewAsset(e.getMessage))
    }
  }

  protected def createNewAssetAction(request: NewRequest,
                                     oldResource: OldResource,
                                     doc: MigrationDoc): Future[String]


}