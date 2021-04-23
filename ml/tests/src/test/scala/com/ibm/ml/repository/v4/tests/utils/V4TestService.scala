/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.utils

import org.apache.http.{Header, HttpEntity}
import spray.json.JsValue

trait V4TestService[EntityClass, ResourceClass, ResourceClasses] {
  def getUri(parentId: Option[String], id: Option[String]): String

  def get(next: Option[String], // if this is set then the other query parameters are ignored (this is the 'next' for pagination)
          include: Option[String],
          assetId: Option[String], // not the resource id - an asset_id that depends on the actual API being called
          tagValue: Iterable[String],
          spaceId: Option[String],
          projectId: Option[String],
          systemRuntimes: Option[Boolean],
          limit: Option[Int],
          finished: Option[Boolean],
          identity: Option[String],
          computeTotalCount: Option[Boolean],
          requestId: Option[String],
          parentId: Option[String],
          expectedStatus: Iterable[Int],
          checkResponse: Boolean,
          id: Option[String]): (ResourceClasses, Array[Header], Int)

  def getAsJson(include: Option[String],
                assetId: Option[String],
                tagValue: Iterable[String],
                spaceId: Option[String],
                projectId: Option[String],
                systemRuntimes: Option[Boolean],
                limit: Option[Int],
                finished: Option[Boolean],
                identity: Option[String],
                computeTotalCount: Option[Boolean],
                requestId: Option[String],
                parentId: Option[String],
                expectedStatus: Iterable[Int],
                checkResponse: Boolean,
                id: Option[String]): (JsValue, Array[Header], Int)

  def createRaw(entity: Option[HttpEntity],
                requestId: Option[String],
                parentId: Option[String],
                spaceId: Option[String],
                projectId: Option[String],
                extraHeaders: Option[Map[String, String]],
                expectedStatus: Iterable[Int],
                checkResponse: Boolean): (JsValue, Array[Header], Int)

  def createJson(entity: Option[JsValue],
                 requestId: Option[String],
                 parentId: Option[String],
                 spaceId: Option[String],
                 projectId: Option[String],
                 extraHeaders: Option[Map[String, String]],
                 expectedStatus: Iterable[Int],
                 checkResponse: Boolean): (JsValue, Array[Header], Int)

  def create(entity: Option[EntityClass],
             requestId: Option[String],
             parentId: Option[String],
             spaceId: Option[String],
             projectId: Option[String],
             extraHeaders: Option[Map[String, String]],
             expectedStatus: Iterable[Int],
             checkResponse: Boolean): (ResourceClass, Array[Header], Int)

  def getById(id: String,
              rev: Option[String],
              spaceId: Option[String],
              projectId: Option[String],
              requestId: Option[String],
              parentId: Option[String],
              expectedStatus: Iterable[Int],
              checkResponse: Boolean): (ResourceClass, Array[Header], Int)

  def getByIdAsJson(id: String,
              rev: Option[String],
              spaceId: Option[String],
              projectId: Option[String],
              requestId: Option[String],
              parentId: Option[String],
              expectedStatus: Iterable[Int],
              checkResponse: Boolean): (JsValue, Array[Header], Int)

  def getByHref(href: String,
                spaceId: Option[String],
                projectId: Option[String],
                requestId: Option[String],
                parentId: Option[String],
                expectedStatus: Iterable[Int],
                checkResponse: Boolean): (ResourceClass, Array[Header], Int)

  def update(id: String,
             patch: JsValue,
             spaceId: Option[String],
             projectId: Option[String],
             requestId: Option[String],
             parentId: Option[String],
             expectedStatus: Iterable[Int],
             checkResponse: Boolean): (ResourceClass, Array[Header], Int)

  def updateRaw(id: String,
                patch: JsValue,
                spaceId: Option[String],
                projectId: Option[String],
                requestId: Option[String],
                parentId: Option[String],
                expectedStatus: Iterable[Int],
                checkResponse: Boolean): (JsValue, Array[Header], Int)

  def delete(id: String,
             spaceId: Option[String],
             projectId: Option[String],
             requestId: Option[String],
             parentId: Option[String],
             expectedStatus: Iterable[Int],
             checkResponse: Boolean,
             queryParams: Map[String, String]): (Option[String], Array[Header], Int)

  def createRevision(id: String,
                      payload: JsValue,
                      spaceId: Option[String],
                      projectId: Option[String],
                      requestId: Option[String],
                      parentId: Option[String],
                      expectedStatus: Iterable[Int],
                      extraHeaders: Option[Map[String, String]],
                      checkResponse: Boolean): (ResourceClass, Array[Header], Int)

  def createRevisionRaw(id: String,
                     payload: JsValue,
                     spaceId: Option[String],
                     projectId: Option[String],
                     requestId: Option[String],
                     parentId: Option[String],
                     expectedStatus: Iterable[Int],
                     extraHeaders: Option[Map[String, String]],
                     checkResponse: Boolean): (JsValue, Array[Header], Int)

  def getRevisions(next: Option[String],
                   spaceId: Option[String],
                   projectId: Option[String],
                   start: Option[String],
                   limit: Option[Int],
                   requestId: Option[String],
                   parentId: Option[String],
                   expectedStatus: Iterable[Int],
                   checkResponse: Boolean,
                   id: Option[String]): (JsValue, Array[Header], Int)

}
