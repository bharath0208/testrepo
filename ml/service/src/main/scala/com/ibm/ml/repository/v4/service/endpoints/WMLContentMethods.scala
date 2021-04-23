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
import com.ibm.analytics.cams.api.v2.assets.AssetAttachmentCompleteResult
import com.ibm.ml.repository.v4.utils.CallContext
import spray.json.JsValue

import scala.concurrent.Future

trait WMLContentMethods {

  type ResourceClasses = JsValue

  def getContent(assetId: String,
                 rev: Option[String],
                 contentFormat: Option[String],
                 name: Option[String],
                 pipelineNodeId: Option[String],
                 deploymentId: Option[String])
                (implicit callContext: CallContext): Future[ResourceClasses]

  def downloadSingleAttachment(assetId: String,
                               rev: Option[String])
                              (implicit callContext: CallContext): Future[HttpResponse]

  def downloadAttachment(assetId: String,
                         attachmentId: String,
                         rev: Option[String])
                        (implicit callContext: CallContext): Future[HttpResponse]

  def downloadAttachmentWithFilter(assetId: String,
                                   rev: Option[String],
                                   contentFormat: Option[String],
                                   name: Option[String],
                                   pipelineNodeId: Option[String],
                                   deploymentId: Option[String])
                                  (implicit callContext: CallContext): Future[HttpResponse]

  def deleteAttachment(assetId: String,
                       attachmentId: String)
                      (implicit callContext: CallContext): Future[Unit]

  def uploadAttachment(assetId: String,
                       assetType: String,
                       dataSource: Source[ByteString, Any],
                       contentFormat: String,
                       contentMIMEType: String,
                       name: Option[String],
                       pipelineNodeId: Option[String],
                       deploymentId: Option[String])
                      (implicit callContext: CallContext): Future[AssetAttachmentCompleteResult]

  def uploadSingleAttachment(assetId: String,
                             assetType: String,
                             contentMIMEType: String,
                             dataSource: Source[ByteString, Any],
                             name: Option[String])
                            (implicit callContext: CallContext): Future[AssetAttachmentCompleteResult]
}
