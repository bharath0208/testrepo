/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.api.internal

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{Directives, Route}
import akka.util.ByteString
import com.ibm.analytics.wml.utils.HeartbeatJsonFormat._
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.endpoints.HeartbeatEndpoint
import com.ibm.ml.repository.v4.utils.APIUtils
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.util.{Failure, Success, Try}

/* the endpoints for the service health probe */
case class HeartbeatAPI(sc: ServiceContext)
                       (specificationVersion: String,
                        implementationVersion: String) extends APIUtils with Directives with StrictLogging {
  /* The endpoints for the health probe API. */
  val endpoints: Route = {
    respondWithDefaultHeaders(getNoCacheResponseHeaders) {
      getHealthProbe
    } ~
      testEndpoint
  }

  /* /ml/wml_services/ml-repository/heartbeat */
  def getHealthProbe: Route = {
    path("ml" / "wml_services" / "ml-repository" / "heartbeat") {
      get {
        parameters("full".as[Boolean] ? false) { full: Boolean =>
          val heartbeat = HeartbeatEndpoint.getHealthProbeApi(sc)(specificationVersion, implementationVersion)
          heartbeat match {
            case Success(ok) =>
              val js = JsObject(
                ok.toJson.asJsObject.fields ++ (if (full) sc.registrationResult.fields else Map())
              )
              complete(
                HttpResponse(
                  status = StatusCodes.OK,
                  entity = HttpEntity(`application/json`, ByteString(js.prettyPrint))
                )
              )
            case Failure(exception) =>
              failWith(exception)
          }
        }
      }
    }
  }

  def testEndpoint: Route = {
    path("ml" / "wml_services" / "ml-repository" / "test") {
      post {
        parameters("timeout".as[Int] ? 0) { timeout: Int =>
          entity(as[String]) {
            _ => {
              if (timeout > 0) {
                Try {
                  Thread.sleep(timeout)
                }
              }
              complete(
                HttpResponse(
                  status = StatusCodes.OK,
                  entity = HttpEntity(`application/json`, ByteString("{}"))
                )
              )
            }
          }
        }
      }
    }
  }
}
