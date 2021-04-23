/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.cams

import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets._
import com.ibm.analytics.wml.cams.{CAMSClient, DataAssetType}
import com.ibm.ml.repository.v4.service.endpoints.WMLContentMethods
import com.ibm.ml.repository.v4.service.utils.CAMSResourceConverterUtils
import com.ibm.ml.repository.v4.utils.{InternalErrorExceptionMessage, NoAttachmentAvailableMessage, ServiceException, TooManyAttachmentAvailableMessage, _}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.{ExecutionContext, Future}

case class CAMSContentMethods(camsClient: CAMSClient)
                             (implicit ec: ExecutionContext) extends WMLContentMethods with StrictLogging with CAMSResourceConverterUtils {
  override def getContent(assetId: String,
                          rev: Option[String],
                          contentFormat: Option[String],
                          name: Option[String],
                          pipelineNodeId: Option[String],
                          deploymentId: Option[String])
                         (implicit callContext: CallContext): Future[ResourceClasses] = {
    for {
      asset <- camsClient.getAssetMetadata(callContext.identity, assetId, rev, callContext.container, callContext.containerStorageType)
    } yield {
      import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
      asset.attachments.getOrElse(Vector[AssetAttachmentMetadata]()).toJson
    }
  }

  override def downloadSingleAttachment(assetId: String,
                                        rev: Option[String])
                                       (implicit callContext: CallContext): Future[HttpResponse] = {
    for {
      asset <- camsClient.getAssetMetadata(callContext.identity, assetId, rev, callContext.container, callContext.containerStorageType)
      _ = if (asset.attachments.map(_.length).getOrElse(0) == 0) {
        throw ServiceException(
          StatusCodes.NotFound,
          NoAttachmentAvailableMessage()
        )
      }
      attachmentId = asset.attachments.get(0).attachmentId
      downloadResponse <- downloadAction(assetId, attachmentId, rev)(callContext)
    } yield {
      downloadResponse
    }
  }

  override def downloadAttachment(assetId: String,
                                  attachmentId: String,
                                  rev: Option[String])
                                 (implicit callContext: CallContext): Future[HttpResponse] = {
    downloadAction(assetId, attachmentId, rev)(callContext)
  }


  override def downloadAttachmentWithFilter(assetId: String,
                                            rev: Option[String],
                                            contentFormat: Option[String],
                                            name: Option[String],
                                            pipelineNodeId: Option[String],
                                            deploymentId: Option[String])
                                           (implicit callContext: CallContext): Future[HttpResponse] = {
    for {
      asset <- camsClient.getAssetMetadata(callContext.identity, assetId, rev, callContext.container, callContext.containerStorageType)
      attachments = filterContentMetadata(contentFormat, name, pipelineNodeId, deploymentId, asset.attachments.getOrElse(Vector[AssetAttachmentMetadata]()))
      _ = if (attachments.isEmpty) {
        throw ServiceException(
          StatusCodes.NotFound,
          NoAttachmentAvailableMessage()
        )
      } else if (attachments.length > 1) {
        throw ServiceException(
          StatusCodes.BadRequest,
          TooManyAttachmentAvailableMessage()
        )
      }
      attachmentId = attachments(0).attachmentId
      downloadResponse <- downloadAction(assetId, attachmentId, rev)(callContext)
    } yield {
      downloadResponse
    }
  }

  private def downloadAction(assetId: String,
                             attachmentId: String,
                             rev: Option[String])
                            (implicit callContext: CallContext): Future[HttpResponse] = {
    for {
      assetAttachmentMeta <- camsClient.getAssetAttachmentMetadata(callContext.identity, attachmentId, assetId, rev, callContext.container, callContext.containerStorageType)
      downloadResponse <- camsClient.downloadAttachmentUsingSignedUrl(callContext.identity, assetAttachmentMeta, DataAssetType.managed)
    } yield {
      downloadResponse
    }
  }

  override def deleteAttachment(assetId: String,
                                attachmentId: String)
                               (implicit callContext: CallContext): Future[Unit] = {
    for {
      _ <- camsClient.deleteAssetAttachment(callContext.identity, assetId, attachmentId, callContext.container, callContext.containerStorageType)
    } yield {
    }
  }

  override def uploadAttachment(assetId: String,
                                assetType: String,
                                dataSource: Source[ByteString, Any],
                                contentFormat: String,
                                contentMIMEType: String,
                                name: Option[String],
                                pipelineNodeId: Option[String],
                                deploymentId: Option[String])
                               (implicit callContext: CallContext): Future[AssetAttachmentCompleteResult] = {
    val pipelineNodeIdField = pipelineNodeId.map { id => List(PIPELINE_NODE_ID -> JsString(id)) }.getOrElse(List())
    val contentFormatField = List(CONTENT_FORMAT -> JsString(contentFormat))
    val deploymentIdField = deploymentId.map { id => List(DEPLOYMENT_ID -> JsString(id)) }.getOrElse(List())
    val fieldList = pipelineNodeIdField ++ contentFormatField ++ deploymentIdField
    val userData = JsObject(fieldList: _*)
    val assetAttachmentInput = AssetAttachmentInput(assetType = assetType, name = name, userData = Some(userData))
    uploadAction(assetId, dataSource, assetAttachmentInput)(callContext)
  }

  override def uploadSingleAttachment(assetId: String,
                                      assetType: String,
                                      contentMIMEType: String,
                                      dataSource: Source[ByteString, Any],
                                      name: Option[String])
                                     (implicit callContext: CallContext): Future[AssetAttachmentCompleteResult] = {
    val assetAttachmentInput = AssetAttachmentInput(assetType = assetType, name = name)
    uploadAction(assetId, dataSource, assetAttachmentInput)(callContext)
  }

  private def uploadAction(assetId: String,
                           dataSource: Source[ByteString, Any],
                           assetAttachmentInput: AssetAttachmentInput)(implicit callContext: CallContext): Future[AssetAttachmentCompleteResult] = {
    for {
      (assetAttachmentMeta, statusCode) <- camsClient.createAssetAttachment(callContext.identity, assetAttachmentInput, assetId, callContext.container, callContext.containerStorageType)
      //Todo handle 202 cases, right now we will handle 202 case by using retry
      result <- uploadAttachmentComplete(assetId, assetAttachmentMeta, dataSource)
    } yield {
      result
    }
  }

  private def uploadAttachmentComplete(id: String,
                                       attachment: AssetAttachmentMetadata,
                                       dataSource: Source[ByteString, Any])
                                      (implicit callContext: CallContext): Future[AssetAttachmentCompleteResult] = {
    {
      for {
        _ <- camsClient.uploadManagedAttachmentWithStream(callContext.identity, attachment, dataSource)
        result <- camsClient.uploadAttachmentComplete(callContext.identity, id, attachment, callContext.container, callContext.containerStorageType)
      } yield {
        if (result.attachmentId.isEmpty) {
          logger.error(s"Failed to get the upload attachment id for asset $id from ${logPrint(result.toJson)}")
          throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(s"Failed to get the upload attachment id for asset $id"))
        }
        result
      }
    } recoverWith {
      case e =>
        logger.error(s"An error occurred during the upload attachment for asset $id")
        // we don't wait for the delete to be finished
        camsClient.deleteAssetAttachment(callContext.identity, id, attachment.attachmentId, callContext.container, callContext.containerStorageType)
        throw e
    }
  }
}
