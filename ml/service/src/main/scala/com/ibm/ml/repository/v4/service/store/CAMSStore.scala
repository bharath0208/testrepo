/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2021
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.store

import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.ibm.analytics.wml.cams.{CAMSClient, DataAssetType, Endpoints}
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}
import com.typesafe.scalalogging.StrictLogging

import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future}

case class CAMSStore(camsClient: CAMSClient,
                     identity: Identity,
                     assetId: String,
                     rev: Option[String],
                     container: Container,
                     containerStorageType: Option[String]) extends FileStore with StrictLogging {


  override def read(attachmentId: String)
                   (implicit ec: ExecutionContext): Future[Source[ByteString, Any]] = {

    for {
      assetAttachmentMeta <- camsClient.getAssetAttachmentMetadata(identity, attachmentId, assetId, rev, container, containerStorageType)
      downloadResponse <- camsClient.downloadAttachmentUsingSignedUrl(identity, assetAttachmentMeta, DataAssetType.managed)
    } yield {
      downloadResponse.entity.dataBytes
    }
  }

  override val storeType: String = "cams"

  override def uri(attachmentId: String): String = Endpoints.assetAttachmentById(assetId, attachmentId) +  getQuery()

  protected def getQuery(): String = {
    val (k, v) = container match {
      case Space(value) => ("space_id", value)
      case Project(value) => ("project_id", value)
    }
    s"?$k=$v" + rev.map(s => s"&revision_id=${URLEncoder.encode(s, "UTF-8")}").getOrElse("")
  }
}
