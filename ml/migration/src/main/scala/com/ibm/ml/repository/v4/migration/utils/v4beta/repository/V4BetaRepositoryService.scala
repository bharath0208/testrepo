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

import com.ibm.analytics.wml.service.utils.security.model.Identity
import spray.json.JsValue

import scala.concurrent.Future

trait V4BetaRepositoryService[ResourceClass, ResourceClasses] {
  def list(identity: Identity,
           spaceId: Option[String],
           projectId: Option[String],
           start: Option[String], // if this is set then the other query parameters are ignored (this is the 'next' for pagination)
           limit: Option[Int],
           tagValue: Option[String],
           extraHeaders: Option[Map[String, String]]): Future[ResourceClasses]

  def listAllIds(identity: Identity,
                 spaceId: Option[String],
                 projectId: Option[String]): Future[Seq[String]]

  def get(identity: Identity,
          id: String,
          rev: Option[String],
          spaceId: Option[String],
          projectId: Option[String],
          extraHeaders: Option[Map[String, String]]): Future[ResourceClass]

  // Julian added this just for the tests
  def createRaw(identity: Identity,
                entity: JsValue,
                spaceId: Option[String],
                projectId: Option[String],
                extraHeaders: Option[Map[String, String]]): Future[JsValue]
}
