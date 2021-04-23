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

import akka.actor.ActorSystem
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Route
import com.ibm.analytics.wml.service.utils.http.{AuthorizationAction, AuthorizationRejection}
import com.ibm.analytics.wml.service.utils.security.StableServiceId
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.service.utils.security.model.Subject.Service
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.security.AuthorizationException
import com.ibm.ml.repository.v4.service.api.v4.AbstractV4API
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.utils._
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class CAMSRegistrationAPI(sc: ServiceContext) extends AbstractV4API(sc) with SprayJsonSupport {
  implicit val system: ActorSystem = sc.downstreamActorSystem
  implicit val builder: HttpClientBuilder = sc.authHttp

  private def isAllowedServiceId(identity: Identity): Boolean = {
    Try((identity.subject.subjectType == Service) && StableServiceId.isStableServiceId(identity.subject.id)) match {
      case Success(allowed) => allowed
      case Failure(_) => false
    }
  }

  val endpoints: Route = {
    respondWithDefaultHeaders(getNoCacheResponseHeaders) {
      getCAMSRegistrationStatus
    } ~
      forceCAMSRegistration
  }

  def getCAMSRegistrationStatus: Route = {
    implicit ctx => {
      path("ml" / "wml_services" / "ml-repository" / "heartbeat" / "registration") {
        get {
          authenticate(ctx) { implicit identity =>
            if (!isAllowedServiceId(identity))
              reject(
                AuthorizationRejection(
                  new AuthorizationException("Not authorized"),
                  identity,
                  AuthorizationAction.Admin.name,
                  None
                )
              )
            enter(this.logger, ctx, identity)
            onComplete {
              Future.successful(sc.registrationResult)
            } {
              case Success(js) =>
                exit(this.logger, ctx, identity)
                complete(
                  HttpResponse(
                    status = StatusCodes.OK,
                    entity = getJsonEntity(js)
                  )
                )
              case Failure(exception) =>
                exit(this.logger, ctx, identity)
                failWith(exception)
            }
          }
        }
      }
    }.apply(ctx)
  }

  def forceCAMSRegistration: Route = {
    implicit ctx => {
      path("ml" / "wml_services" / "ml-repository" / "heartbeat" / "registration" / "force") {
        post {
          authenticate(ctx) { implicit identity =>
            if (!isAllowedServiceId(identity))
              reject(
                AuthorizationRejection(
                  new AuthorizationException("Not authorized"),
                  identity,
                  AuthorizationAction.Admin.name,
                  None
                )
              )
            enter(this.logger, ctx, identity)
            onComplete {
              sc.registrationResult = JsObject(
                "cams_registration" -> JsString("none")
              )
              implicit val ec: ExecutionContext = sc.downstreamActorSystem.dispatcher
              val date: String = formatAsDate(System.currentTimeMillis())
              for {
                result <- sc.getRegisterCAMSGlobalAssetsJob(sc.downstreamActorSystem, s"force-cams-registration-$date")
              } yield {
                val json = JsObject(
                  "cams_registration" -> result.toJson,
                  "registered_at" -> JsString(date)
                )
                sc.registrationResult = json
                json
              }
            } {
              case Success(js) =>
                exit(this.logger, ctx, identity)
                complete(
                  HttpResponse(
                    status = StatusCodes.OK,
                    entity = getJsonEntity(js)
                  )
                )
              case Failure(exception) =>
                exit(this.logger, ctx, identity)
                failWith(exception)
            }
          }
        }
      }
    }.apply(ctx)
  }
}
