/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources

import com.ibm.analytics.cams.api.v2.assets.Asset
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.utils.{CallContext, enableContentCheck}
import spray.json.JsValue

import scala.concurrent.{ExecutionContext, Future}

trait ContentValidator extends ValidateUtils {

  def validateUploadContentType(contentType: String, uploadContentTypesAllowed: List[String]): Unit = {
    if (!uploadContentTypesAllowed.contains(contentType)) {
      if (enableContentCheck) {
        contentTypeNotAllowedError(contentType, uploadContentTypesAllowed.mkString(", "))
      }
    }
  }

  def validateUpload(entity: JsValue, contentFormat: String, contentMimeType: String, name: Option[String], pipelineNodeId: Option[String], deploymentId: Option[String])(implicit callContext: CallContext): Future[(String, Option[String], Option[String], Option[String])] = {
    // please over write this function
    Future.successful(contentFormat, name, pipelineNodeId, deploymentId)
  }

  def validateSingleUpload(resource: Asset, contentMimeType: String, camsClient: CAMSClient)(implicit callContext: CallContext, ec: ExecutionContext): Future[Unit] = {
    for (
      // check if content already exist, if true we need to delete it
      _ <- deleteAllAttachmentsIfExist(resource, camsClient)
    ) yield {}
  }

}
