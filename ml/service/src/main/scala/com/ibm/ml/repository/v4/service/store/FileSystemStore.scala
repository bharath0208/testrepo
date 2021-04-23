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

import akka.stream.scaladsl.{Source, StreamConverters}
import akka.util.ByteString
import com.typesafe.scalalogging.StrictLogging

import java.io.{BufferedInputStream, File, FileInputStream}
import scala.concurrent.{ExecutionContext, Future}

case class FileSystemStore(rootPath: String) extends FileStore with StrictLogging {
  private def resolvePath(relativePath: String): String = {
    rootPath + addBackSlashPrefix(relativePath)
  }

  override def read(path: String)
                   (implicit ec: ExecutionContext): Future[Source[ByteString, Any]] = Future {
    val absPath = new File(resolvePath(path))
    StreamConverters.fromInputStream(() => new BufferedInputStream(new FileInputStream(absPath)))
  }

  override val storeType: String = "FS"

  override def uri(relativePath: String): String = resolvePath(relativePath)
}
