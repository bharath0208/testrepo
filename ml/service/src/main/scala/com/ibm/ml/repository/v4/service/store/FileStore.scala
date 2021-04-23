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

import akka.stream.scaladsl.Source
import akka.util.ByteString

import scala.concurrent.{ExecutionContext, Future}

trait FileStore {
  val storeType: String // something like "COS" or "fs"
  val host: String = ""
  val port: Int = -1

  def uri(relativePath: String): String

  def read(relativePath: String)
          (implicit ec: ExecutionContext): Future[Source[ByteString, Any]]

  protected def addBackSlashPrefix(relativePath: String): String =
    if (relativePath.startsWith("/")) relativePath else s"/$relativePath"
}
