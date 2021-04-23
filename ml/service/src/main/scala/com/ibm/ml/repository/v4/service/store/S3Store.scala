/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.store

import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path
import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import com.amazonaws.auth.BasicAWSCredentials
import com.amazonaws.services.s3.AmazonS3Client
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

case class S3Store(accessKey: String,
                   secretKey: String,
                   url: String,
                   bucket: String) extends FileStore with StrictLogging {
  val cos = new AmazonS3Client(new BasicAWSCredentials(accessKey, secretKey))
  cos.setEndpoint(url)

  override def read(path: String)
                   (implicit ec: ExecutionContext): Future[Source[ByteString, Any]] = Future {
    val relativePath = if (path.startsWith("/")) path.substring(1) else path
    val inputStream = cos.getObject(bucket, relativePath).getObjectContent
    StreamConverters.fromInputStream(() => inputStream)
  }

  override val storeType: String = cos.getServiceName // "S3"

  override val host: String = Uri(url).authority.host.toString()

  override val port: Int = Uri(url).authority.port

  override def uri(relativePath: String): String = {
    val path = addBackSlashPrefix(relativePath)
    Uri(url).withPath(Path(path)).path.toString()
  }
}
