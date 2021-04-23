/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils.v4beta.repository

import akka.http.scaladsl.model.ContentType
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.ibm.analytics.wml.service.utils.security.model.Identity

import scala.concurrent.Future

trait V4BetaRepositorySingleContentService[ResourceClass, ResourceClasses]
  extends V4BetaRepositoryService[ResourceClass, ResourceClasses] {

  def download(identity: Identity,
               id: String,
               rev: Option[String],
               format: Option[String],
               artifact: Option[String],
               spaceId: Option[String],
               projectId: Option[String],
               extraHeaders: Option[Map[String, String]]): Future[Source[ByteString, Any]]
}
