/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.endpoints

import akka.http.scaladsl.model.HttpResponse
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetAttachmentCompleteResult}
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.ml.repository.v4.service.resources.AbstractResourceWithContent
import com.ibm.ml.repository.v4.utils.CallContext
import spray.json._

import scala.concurrent.{ExecutionContext, Future}

case class AbstractContentMethods[EntityRequest, Resource, Resources]
(resource: AbstractResourceWithContent[EntityRequest, Resource, Resources])
(implicit ec: ExecutionContext,
 requestJF: RootJsonFormat[EntityRequest],
 resourceJF: RootJsonFormat[Resource],
 resourcesJF: RootJsonFormat[Resources]) extends WMLContentMethods {

  override def getContent(assetId: String,
                          rev: Option[String],
                          contentFormat: Option[String],
                          name: Option[String],
                          pipelineNodeId: Option[String],
                          deploymentId: Option[String])
                         (implicit callContext: CallContext): Future[ResourceClasses] = {
    for {
      content <- resource.camsContent.getContent(assetId, rev, contentFormat, name, pipelineNodeId, deploymentId)
    } yield {
      resource.convertCAMSResourceContent(content, contentFormat, name, pipelineNodeId, deploymentId).toJson
    }

  }

  override def downloadSingleAttachment(assetId: String,
                                        rev: Option[String])
                                       (implicit callContext: CallContext): Future[HttpResponse] = {
    resource.camsContent.downloadSingleAttachment(assetId, rev)
  }

  override def downloadAttachment(assetId: String,
                                  attachmentId: String,
                                  rev: Option[String])
                                 (implicit callContext: CallContext): Future[HttpResponse] = {
    resource.camsContent.downloadAttachment(assetId, attachmentId, rev)
  }


  override def downloadAttachmentWithFilter(assetId: String,
                                            rev: Option[String],
                                            contentFormat: Option[String],
                                            name: Option[String],
                                            pipelineNodeId: Option[String],
                                            deploymentId: Option[String])
                                           (implicit callContext: CallContext): Future[HttpResponse] = {
    resource.camsContent.downloadAttachmentWithFilter(assetId, rev, contentFormat, name, pipelineNodeId, deploymentId)
  }


  override def deleteAttachment(assetId: String,
                                attachmentId: String)
                               (implicit callContext: CallContext): Future[Unit] = {
    resource.camsContent.deleteAttachment(assetId, attachmentId)
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
    for {
      camsResource <- resource.camsResource.get(assetId, None)
      (trimmedContentFormat, trimmedName, trimmedPipelineNodeId, trimmedDeploymentId) <- resource.validateUpload(camsResource, contentFormat, contentMIMEType, name, pipelineNodeId, deploymentId)
      result <- resource.camsContent.uploadAttachment(assetId, assetType, dataSource, trimmedContentFormat, contentMIMEType, trimmedName, trimmedPipelineNodeId, trimmedDeploymentId)
    } yield {
      result
    }
  }

  override def uploadSingleAttachment(assetId: String,
                                      assetType: String,
                                      contentMIMEType: String,
                                      dataSource: Source[ByteString, Any],
                                      name: Option[String])
                                     (implicit callContext: CallContext): Future[AssetAttachmentCompleteResult] = {
    for {
      camsResource <- resource.camsResource.get(assetId, None)
      _ <- resource.validateSingleUpload(camsResource.convertTo[Asset], contentMIMEType, resource.camsContent.camsClient)
      result <- resource.camsContent.uploadSingleAttachment(assetId, assetType, contentMIMEType, dataSource, name)
    } yield {
      result
    }
  }
}
