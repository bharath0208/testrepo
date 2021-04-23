/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.errors

import akka.http.javadsl.server.{MissingQueryParamRejection, RequestEntityExpectedRejection}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.HttpChallenge
import akka.http.scaladsl.server._
import com.ibm.analytics.wml.service.utils.http.{AuthRejectionHandler, AuthenticationRejection}
import com.ibm.analytics.wml.utils.errors.{MLFailure, MLFailures, Target}
import com.ibm.analytics.wml.utils.security.http.MLAuthenticationRejection
import com.ibm.ml.repository.v4.utils.logging.AccessLogger
import com.ibm.ml.repository.v4.utils.{AUTH_IAM, AUTH_ICP, getRequestId, getWmlAuth, _}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

object ServiceRejectionHandler extends StrictLogging {
  private def getAuthenticationHeader: Seq[HttpHeader] = {
    getWmlAuth match {
      case AUTH_ICP =>
        Vector(headers.`WWW-Authenticate`(HttpChallenge(scheme = "Basic", realm = Some(AUTH_ICP.name))))
      case AUTH_IAM =>
        Vector(headers.`WWW-Authenticate`(HttpChallenge(scheme = "Bearer", realm = Some(AUTH_IAM.name))))
      case _ =>
        Vector(headers.`WWW-Authenticate`(HttpChallenge(scheme = "Bearer", realm = None)))
    }
  }

  def getStatus(rejection: Rejection): StatusCode = {
    rejection match {
      case _: RequestEntityExpectedRejection => StatusCodes.BadRequest
      case _: MissingQueryParamRejection => StatusCodes.BadRequest
      case _: ValidationRejection => StatusCodes.BadRequest
      case _: MalformedRequestContentRejection => StatusCodes.BadRequest
      case _: UnsupportedRequestContentTypeRejection => StatusCodes.UnsupportedMediaType
      case _: MethodRejection => StatusCodes.MethodNotAllowed
      case _: MLAuthenticationRejection => StatusCodes.Unauthorized // keep his here for the migration calling v3 APIs for now
      case _: AuthenticationRejection => StatusCodes.Unauthorized
      case _ => StatusCodes.InternalServerError
    }
  }

  def apply(): RejectionHandler = getGlobalRejectionHandler withFallback getNotFoundRejectionHandler withFallback AuthRejectionHandler() withFallback getFallbackRejectionHandler

  private def getFailures(status: StatusCode,
                          failures: Seq[MLFailure],
                          request: HttpRequest): String = {
    MLFailures(
      errors = failures,
      trace = getRequestId(request).getOrElse("unknown"),
      statusCode = Some(status.intValue())
    ).toJson.prettyPrint
  }

  private def getTarget(e: MalformedRequestContentRejection): Option[Target] = {
    if (e.cause != null) {
      e.cause match {
        case e: spray.json.DeserializationException =>
          ServiceException.getTarget(e)
        case _ =>
          None
      }
    } else
      None
  }

  def getGlobalRejectionHandler: RejectionHandler = {
    RejectionHandler.newBuilder()
      .handle {
        case r: RequestEntityExpectedRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(r)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                NoContentMessage().asFailure
              ),
              ctx.request
            )
          )
        case e: MissingQueryParamRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(e)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                MissingQueryParameterMessage(e.parameterName).asFailure
              ),
              ctx.request
            )
          )
        case vr: ValidationRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(vr)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                ContentValidationFailedMessage(vr.message).asFailure
              ),
              ctx.request
            )
          )
        case e: MalformedRequestContentRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(e)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                MalformedRequestContentMessage(e.message, getTarget(e)).asFailure
              ),
              ctx.request
            )
          )
        case e: UnsupportedRequestContentTypeRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(e)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                UnknownContentTypeMessage(e.supported.mkString(",")).asFailure
              ),
              ctx.request
            )
          )
        case mr: MethodRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(mr)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                MethodNotAllowedMessage(ctx.request.method.value, mr.supported.value).asFailure
              ),
              ctx.request
            )
          )
        // keep his here for the migration calling v3 APIs for now
        case ae: MLAuthenticationRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(ae)
          ctx.complete(
            status,
            getAuthenticationHeader,
            getFailures(
              status,
              Seq(
                MLFailure("authorization_rejected", ae.cause.getMessage, Some(Target("header", "Authorization")), None)
              ),
              ctx.request
            )
          )
        case ae: AuthenticationRejection => ctx =>
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(ae)
          ctx.complete(
            status,
            getAuthenticationHeader,
            getFailures(
              status,
              Seq(
                MLFailure("authorization_rejected", ae.cause.getMessage, Some(Target("header", "Authorization")), None)
              ),
              ctx.request
            )
          )
      }
      .result()
  }

  def getFallbackRejectionHandler: RejectionHandler = {
    // this is here to ensure that the request is discarded and that
    // we send back a valid json error
    RejectionHandler.newBuilder()
      .handle {
        case fallback: Rejection => ctx =>
          logger.info(s"Unhandled rejection ${fallback.getClass.getName} - returning an internal server error!")
          ctx.request.discardEntityBytes(ctx.materializer)
          val status = getStatus(fallback)
          ctx.complete(
            status,
            getFailures(
              status,
              Seq(
                InternalErrorExceptionMessage(fallback.toString).asFailure
              ),
              ctx.request
            )
          )
      }
      .result()
  }

  private def logResponse(ctx: RequestContext, status: StatusCode): StatusCode = {
    AccessLogger.logAccess(ctx.request, None, status.intValue())
    status
  }

  def getNotFoundRejectionHandler: RejectionHandler = {
    RejectionHandler.newBuilder()
      .handleNotFound { ctx =>
        ctx.request.discardEntityBytes(ctx.materializer)
        val status = logResponse(ctx, StatusCodes.NotFound)
        ctx.complete(
          status,
          getFailures(
            status,
            Seq(
              NotFoundMessage(ctx.request.uri.path.toString()).asFailure
            ),
            ctx.request
          )
        )
      }
      .result()
  }
}
