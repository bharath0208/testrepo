/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.reporting

import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.ml.repository.v4.service.app.MLRepositoryServer
import com.ibm.ml.repository.v4.service.reporting.ReportingAgent.ActionType
import com.ibm.ml.repository.v4.utils._
import spray.json._

import scala.util.{Failure, Success}

case class SegmentEvent(objectType: String, // the asset name
                        `object`: Option[String], // the asset id
                        action: Option[String],
                        instanceId: Option[String],
                        productId: Option[String],
                        productTitle: Option[String],
                        productVersion: Option[String],
                        resultValue: Option[String],
                        environment: Option[String], // The deployment environment name or hostname
                        query: Option[String]) // The query when searching

object SegmentEvent extends DefaultJsonProtocol {
  def report(actionType: ActionType, code: Int): String = {
    def result: String = if (code == StatusCodes.Accepted.intValue)
      "pending"
    else if (code >= StatusCodes.BadRequest.intValue)
      "failed"
    else
      "successful"

    s"$actionType $result"
  }

  implicit val segmentEventFormat: RootJsonFormat[SegmentEvent] = jsonFormat10(SegmentEvent.apply)

  private lazy val wmlEnvironment: Option[String] = {
    wmlPublicHost match {
      case Success(host) => Some(host)
      case Failure(_) => None
    }
  }

  private lazy val apiVersion: Option[String] = ClassUtils.attributeFromMetaOf[MLRepositoryServer]("Specification-Version") match {
    case Success(version) => Some(version)
    case Failure(_) => None
  }

  def apply(identity: Identity,
            instanceId: Option[String],
            request: HttpRequest,
            state: MLRepositoryState,
            httpResponseCode: Int): SegmentEvent = {
    SegmentEvent(
      objectType = state.assetType,
      `object` = Some(state.assetId),
      action = Some(state.actionName),
      instanceId = instanceId,
      productId = Some("5725-W78"),
      productTitle = Some("Watson Machine Learning Service"),
      productVersion = apiVersion,
      resultValue = Some(SegmentEvent.report(state.actionType, httpResponseCode)),
      environment = wmlEnvironment,
      query = if (state.actionType == ReportingAgent.search) Some(request.uri.path.toString()) else None
    )
  }
}
