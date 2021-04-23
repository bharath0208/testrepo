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

import akka.http.scaladsl.model.StatusCode
import com.ibm.ml.repository.v4.utils.CallContext
import spray.json.JsValue

import scala.concurrent.Future

/**
 * These are the methods that a WML resource supports.
 */
trait WMLResourceMethods {
  type EntityClass = JsValue
  type RevisionEntityClass = JsValue
  type ResourceClass = JsValue
  type ResourceClasses = JsValue

  def getAll(start: Option[String],
             limit: Option[Int],
             tagValue: Option[String],
             search: Option[String])
            (implicit callContext: CallContext): Future[ResourceClasses]

  def create(entity: EntityClass)
            (implicit callContext: CallContext): Future[(ResourceClass, StatusCode)]

  def get(id: String,
          rev: Option[String])
         (implicit callContext: CallContext): Future[ResourceClass]

  def update(id: String,
             patch: JsValue)
            (implicit callContext: CallContext): Future[ResourceClass]

  def delete(id: String,
             purgeOnDelete: Boolean)
            (implicit callContext: CallContext): Future[Unit]

  def getAllRevisions(id: String,
                      start: Option[String],
                      limit: Option[Int])
                     (implicit callContext: CallContext): Future[ResourceClasses]

  def createRevision(id: String,
                     entity: RevisionEntityClass)
                    (implicit callContext: CallContext): Future[(ResourceClass, StatusCode)]
}
