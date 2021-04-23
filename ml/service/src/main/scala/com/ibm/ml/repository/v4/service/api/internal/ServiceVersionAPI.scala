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
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import akka.util.ByteString
import com.ibm.analytics.wml.utils.ServiceDetails
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.utils.APIUtils
import spray.json._

import scala.util.{Failure, Success, Try}

case class ServiceVersionAPI(sc: ServiceContext) extends APIUtils with Directives {
  /* The endpoints for the service version API. */
  val endpoints: Route = {
    respondWithDefaultHeaders(getNoCacheResponseHeaders) {
      getServiceVersion
    }
  }

  /* /ml/wml_services/version */
  def getServiceVersion: Route = {
    path("ml" / "wml_services" / "version") {
      get {
        val version: Try[ServiceDetails] = Try(
          ServiceDetails()
        )
        version match {
          case Success(ok) =>
            complete(
              HttpResponse(
                status = StatusCodes.OK,
                entity = HttpEntity(`application/json`, ByteString(ok.toJson.prettyPrint))
              )
            )
          case Failure(exception) =>
            failWith(exception)
        }
      }
    }
  }
}
