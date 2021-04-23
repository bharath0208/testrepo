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

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.cams.api.v2.assets.AssetAttachmentMetadata
import com.ibm.analytics.wml.api.v4ga.common.ContentResources
import com.ibm.ml.repository.v4.service.utils.CAMSResourceConverterUtils
import com.ibm.ml.repository.v4.utils.{FailedConvertCAMSResourceContent, ServiceException, logPrint}
import spray.json.JsValue

import scala.util.{Failure, Success, Try}

trait ContentConverter extends CAMSResourceConverterUtils {
  def convertCAMSResourceContent(resource: JsValue,
                                 contentFormat: Option[String],
                                 name: Option[String],
                                 pipelineNodeId: Option[String],
                                 deploymentId: Option[String]): ContentResources = {
    Try {
      logger.trace(s"start to convert CAMS resource content ${logPrint(resource)}")
      import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
      val metadataList = resource.convertTo[Vector[AssetAttachmentMetadata]]
      val attachments = filterContentMetadata(contentFormat, name, pipelineNodeId, deploymentId, metadataList)
      val totalCount = attachments.length
      ContentResources(totalCount, attachments)
    } match {
      case Success(contentResources) =>
        contentResources
      case Failure(ex) =>
        logger.error(s"An error occurred inside convertCAMSResourceContent: ${ex.getMessage}")
        throw ServiceException(StatusCodes.InternalServerError, FailedConvertCAMSResourceContent(ex))
    }
  }
}
