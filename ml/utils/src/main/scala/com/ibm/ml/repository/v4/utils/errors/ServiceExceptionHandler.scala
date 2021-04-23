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

import java.util.concurrent.TimeoutException

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{ExceptionHandler, RequestContext}
import akka.util.ByteString
import com.ibm.analytics.wml.service.utils.http.AuthExceptionHandler
import com.ibm.analytics.wml.utils.clients.http.{HttpClientError, HttpError}
import com.ibm.analytics.wml.utils.errors.{MLFailure, MLFailures}
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils.logging.{AccessLogger, ExceptionLogger, reqId}
import com.ibm.ml.repository.v4.utils.{DownstreamError, ServiceException, config, formatAsDuration, getRequestId, logPrint}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

object ServiceExceptionHandler extends StrictLogging {
  private val UNKNOWN_STATUS_CODE: Int = StatusCodes.InternalServerError.intValue
  private val UNKNOWN_REQUEST_ID: String = "unknown"

  private def getReqId(requestId: Option[String]): String = {
    requestId.getOrElse(UNKNOWN_REQUEST_ID)
  }

  private def getTimeoutError(request: Option[HttpRequest],
                              duration: Option[Long]): MLFailure = {
    val msg = s"""The server was not able to produce a timely response${getDurationInSeconds(duration," after ")}
                 |, please try again in a short while.""".stripMargin.replaceAll("\n", "")
    MLFailure(
      code = "request-timeout",
      message = msg,
      target = None,
      moreInfo = getApiDocsUrl(request)
    )
  }

  def getTimeoutResponse(requestId: Option[String],
                         request: Option[HttpRequest],
                         duration: Option[Long],
                         log: Boolean = true): HttpResponse = {
    val err = MLFailures(
      errors = Seq(getTimeoutError(request, duration)),
      trace = getReqId(requestId),
      statusCode = Some(StatusCodes.ServiceUnavailable.intValue)
    )
    if (log && request.isDefined) {
      // this is called by the akka http server code so we log it here
      ExceptionLogger.log(Some(s"Timeout${getDuration(duration," after ")}"), new Exception(request.get.uri.toString()), requestId)
    }
    toHttp(err)
  }

  private def getDurationInSeconds(duration: Option[Long],
                          prefix: String = "",
                          postfix: String = "",
                          none: String = ""): String = {
    duration match {
      case Some(dur) =>
        getMessage(Some(utils.getDurationInSeconds(dur)), prefix, postfix, none)
      case None => getMessage(None, prefix, postfix, none)
    }
  }

  private def getDuration(duration: Option[Long],
                          prefix: String = "",
                          postfix: String = "",
                          none: String = ""): String = {
    duration match {
      case Some(dur) => getMessage(Some(formatAsDuration(dur)), prefix, postfix, none)
      case None => getMessage(None, prefix, postfix, none)
    }
  }

  private def getMessage(msg: Option[AnyRef],
                         prefix: String = "",
                         postfix: String = "",
                         none: String = ""): String = {
    msg match {
      case Some(m) => s"$prefix$m$postfix"
      case None => none
    }
  }

  private def getTimeoutDuration(te: TimeoutException): Option[Long] = {
    // do this for now
    logger.info(s"Should get timeout from ${te.getMessage}")
    None
  }

  @scala.annotation.tailrec
  private def getFailures(t: Throwable,
                          failures: Seq[MLFailure],
                          request: HttpRequest,
                          requestId: Option[String]): Seq[MLFailure] = {
    val f: Seq[MLFailure] = t match {
      case se: ServiceException =>
        val more: Option[String] = se.moreInfo match {
          case Some(m) => Some(m)
          case None => getApiDocsUrl(Some(request))
        }
        se.getFailures(more) ++ failures
      case _: NotImplementedError =>
        failures :+ MLFailure(
          code = "not_implemented",
          message = "This method is not yet implemented - contact IBM for more information",
          target = None,
          moreInfo = getApiDocsUrl(Some(request))
        )
      case hce: HttpClientError =>
        failures :+ MLFailure(
          code = "http_client_error",
          message = hce.getMessage,
          target = None,
          moreInfo = getApiDocsUrl(Some(request))
        )
      case te: java.util.concurrent.TimeoutException =>
        failures :+ getTimeoutError(Some(request), getTimeoutDuration(te))
      case t: Throwable if !t.isInstanceOf[Error] =>
        failures :+ MLFailure(
          code = "unknown_error",
          message = s"An unknown error occurred (${t.getMessage})",
          target = None,
          moreInfo = getApiDocsUrl(Some(request))
        )
    }
    if (t.getCause != null)
      getFailures(t.getCause, f, request, requestId)
    else
      f
  }

  @scala.annotation.tailrec
  private def getRawErrorResponse(e: Any,
                                  request: HttpRequest,
                                  requestId: Option[String]): MLFailures = {
    e match {
      case failures: MLFailures =>
        failures
      case mlf: MLFailure =>
        logger.error(s"Exception handler received a MLFailure and not a MLFailures: ${logPrint(mlf.toJson)}")
        // don't set a status code here
        MLFailures(
          errors = Seq(mlf),
          trace = getReqId(requestId)
        )
      case se: ServiceException =>
        MLFailures(
          errors = getFailures(
            se,
            Vector(),
            request,
            requestId
          ),
          trace = getReqId(requestId),
          statusCode = Some(se.status.intValue)
        )
      case he: HttpError =>
        // do this for now
        getRawErrorResponse(he.getCause, request, requestId)
      case hce: HttpClientError =>
        MLFailures(
          errors = getFailures(
            hce,
            Vector(),
            request,
            requestId
          ),
          trace = getReqId(requestId),
          statusCode = Some(hce.status.intValue)
        )
      case nie: NotImplementedError =>
        MLFailures(
          errors = getFailures(
            nie,
            Vector(),
            request,
            requestId
          ),
          trace = getReqId(requestId),
          statusCode = Some(StatusCodes.NotImplemented.intValue)
        )
      case te: java.util.concurrent.TimeoutException =>
        val resp = getTimeoutResponse(requestId = requestId, request = Some(request), getTimeoutDuration(te), log = false)
        MLFailures(
          errors = getFailures(
            te,
            Vector(),
            request,
            requestId
          ),
          trace = getReqId(requestId),
          statusCode = Some(resp.status.intValue)
        )
      case e: Exception =>
        MLFailures(
          errors = getFailures(
            e,
            Vector(),
            request,
            requestId
          ),
          trace = getReqId(requestId),
          statusCode = Some(UNKNOWN_STATUS_CODE.intValue)
        )
      case t: Throwable if !t.isInstanceOf[Error] =>
        MLFailures(
          errors = getFailures(
            t,
            Vector(),
            request,
            requestId
          ),
          trace = getReqId(requestId),
          statusCode = Some(UNKNOWN_STATUS_CODE.intValue)
        )
    }
  }

  private def logResponse(ctx: RequestContext,
                          response: HttpResponse): HttpResponse = {
    AccessLogger.logAccess(ctx.request, None, response.status.intValue())
    response
  }

  private def toString(request: HttpRequest): String = {
    s"${request.method.value} ${request.uri} ${request.entity}"
  }

  private def trimFailures(failures: MLFailures): MLFailures = {
    // remove any duplicates
    var trimmed: Seq[MLFailure] = Seq[MLFailure]()
    def matches(error: MLFailure): Boolean = {
      // we match by only comparing the code and message
      for (t <- trimmed) {
        if ((t.code == error.code) &&
          (t.message == error.message)) {
          return true
        }
      }
      false
    }
    for (error <- failures.errors) {
      if (!matches(error))
        trimmed = trimmed ++ Seq(error)
    }
    if (trimmed.length != failures.errors.length)
      failures.copy(errors = trimmed)
    else
      failures
  }

  private def getInferredStatusCode(errors: Seq[MLFailure]): Option[Int] = {
    def getCode(failure: MLFailure): Option[Int] = {
      if (failure.code.toLowerCase.contains("does_not_exist") )
        Some(StatusCodes.NotFound.intValue)
      else if (failure.code.toLowerCase.contains("invalid_parameter"))
        Some(StatusCodes.BadRequest.intValue)
      else if (failure.code.toLowerCase.contains("connection_error"))
        Some(StatusCodes.ServiceUnavailable.intValue)
      else if (failure.code == "ReservedValue")
        Some(StatusCodes.BadRequest.intValue) // this is when an asset is not in the catalog (CAMS)
      else
        None
    }

    for (error <- errors.reverse) {
      getCode(error) match {
        case Some(status) =>
          return Some(status)
        case None =>
        // continue
      }
    }

    None
  }

  private def getDownstreamError(errors: Seq[MLFailure]): Option[String] = {
    for (error <- errors.reverse) {
      if (error.code.toLowerCase.contains("downstream")) {
        return Some(error.code)
      }
    }
    for (error <- errors.reverse) {
      if (error.code.toLowerCase.contains("spaces")) {
        return Some("downstream_error_spaces")
      }
    }
    for (error <- errors.reverse) {
      if (error.message.toLowerCase.contains("cats".toLowerCase)) {
        return Some("downstream_error_cams")
      }
    }
    None
  }

  // used by unit tests
  def getErrorResponseFromFailures(err: MLFailures,
                                   moreInfo: Option[String]): MLFailures = {
    def getCode(failures: MLFailures, status: Int): Int = {
      failures.statusCode match {
        case Some(code) if code == 410 => 404 // treat any 410 as 404
        case Some(code) => code
        case None => status
      }
    }
    // get the status code if we don't have it - do this before we check for downstream errors
    val status: Int = err.statusCode.getOrElse(getInferredStatusCode(err.errors).getOrElse(UNKNOWN_STATUS_CODE))
    // see if this is a downstream error
    val cleaned: MLFailures = getDownstreamError(err.errors) match {
      case Some(downstream) =>
        val downstreamError = DownstreamError(downstream, status).asFailure
        val more = downstreamError.moreInfo match {
          case Some(mi) => Some(mi)
          case None => moreInfo
        }
        err.copy(errors = Seq(downstreamError.copy(moreInfo = more)) ++ err.errors)
      case None =>
        err
    }
    // now make sure that we have a status code
    trimFailures(cleaned.copy(statusCode = Some(getCode(cleaned, status))))
  }

  private def getErrorResponse(e: Any,
                               request: HttpRequest,
                               requestId: Option[String]): HttpResponse = {
    val err: MLFailures = trimFailures(getRawErrorResponse(e, request, requestId))
    val moreInfo: Option[String] = getApiDocsUrl(Some(request))
    val sanitized = getErrorResponseFromFailures(err, moreInfo)
    logger.debug(s"Returning error ${logPrint(sanitized.toJson)}")
    toHttp(sanitized)
  }

  def toHttp(failures: MLFailures, statusCode: Int = UNKNOWN_STATUS_CODE): HttpResponse = {
    val status: Int = failures.statusCode.getOrElse(statusCode)
    val entity: String = Try(failures.toJson.prettyPrint) match {
      case Success(entity) => entity
      case Failure(exception) => exception.getMessage
    }
    // https://github.ibm.com/NGP-TWC/ml-planning/issues/21033
    // we do an explicit conversion of the status in case that it is a custom status code
    // this is to avoid errors like this:
    // java.lang.RuntimeException: Non-standard status codes cannot be created by implicit conversion. Use `StatusCodes.custom` instead.
    // at akka.http.scaladsl.model.StatusCode$.$anonfun$int2StatusCode$1(StatusCode.scala:29) ~[com.typesafe.akka.akka-http-core_2.13-10.2.2.jar:10.2.2]
    //  at scala.Option.getOrElse(Option.scala:201) ~[org.scala-lang.scala-library-2.13.4.jar:?]
    //  at akka.http.scaladsl.model.StatusCode$.int2StatusCode(StatusCode.scala:28) ~[com.typesafe.akka.akka-http-core_2.13-10.2.2.jar:10.2.2]
    //  at com.ibm.ml.repository.v4.utils.errors.ServiceExceptionHandler$.toHttp(ServiceExceptionHandler.scala:363) ~[com.ibm.ml.repository.v4.utils-2.1.173.jar:2.1.173]
    val returnStatusCode: StatusCode = Try(StatusCode.int2StatusCode(status)) match {
      case Success(sc) =>
        sc
      case Failure(_) =>
        logger.info(s"Handling custom status code $status")
        // must be a custom code
        val (reason: String, defaultMessage: String) = {
          if (entity.toLowerCase.contains("cloudflare")) {
            /* https://en.wikipedia.org/wiki/List_of_HTTP_status_codes#5xx_server_errors
            520 Web Server Returned an Unknown Error
              The origin server returned an empty, unknown, or unexplained response to Cloudflare.[91]
            521 Web Server Is Down
              The origin server has refused the connection from Cloudflare.
            522 Connection Timed Out
              Cloudflare could not negotiate a TCP handshake with the origin server.
            523 Origin Is Unreachable
              Cloudflare could not reach the origin server; for example, if the DNS records for the origin server are incorrect.
            524 A Timeout Occurred
              Cloudflare was able to complete a TCP connection to the origin server, but did not receive a timely HTTP response.
            525 SSL Handshake Failed
              Cloudflare could not negotiate a SSL/TLS handshake with the origin server.
            526 Invalid SSL Certificate
              Cloudflare could not validate the SSL certificate on the origin web server.
            527 Railgun Error
              Error 527 indicates an interrupted connection between Cloudflare and the origin server's Railgun server.[92]
            530
              Error 530 is returned along with a 1xxx error.
             */
            // should we expose the cloudflare messages - I don't think that we have the choice if we want meaningful messages
            status match {
              case 522 =>
                ("Connection Timed Out", "Cloudflare could not negotiate a TCP handshake with the origin server")
              case 524 =>
                ("A Timeout Occurred", "Cloudflare was able to complete a TCP connection to the origin server, but did not receive a timely HTTP response")
              case sc =>
                (s"$sc", "")
            }
          } else {
            (s"$status", "")
          }
        }
        if (status > 599) {
          // allowsEntity true because we return the json error
          StatusCodes.custom(status, reason = reason, defaultMessage = defaultMessage, isSuccess = false, allowsEntity = true)
        } else {
          StatusCodes.custom(status, reason = reason, defaultMessage = defaultMessage)
        }
    }
    HttpResponse(
      returnStatusCode,
      entity = HttpEntity.Strict(
        `application/json`,
        ByteString(entity)
      )
    )
  }

  // Under no circumstances must this method throw an error
  def getStatusCode(e: Any): Int = {
    getErrorResponse(e, HttpRequest(uri = Uri("/ml/v4/dummy")), None).status.intValue()
  }

  // Under no circumstances must this method throw an error
  def apply(): ExceptionHandler = AuthExceptionHandler() withFallback ExceptionHandler {
    case failures: MLFailures => ctx =>
      val requestId = getRequestId(ctx.request)
      reqId(requestId)(() => logger.debug(s"MLFailures: ${logPrint(failures.toJson)} for ${toString(ctx.request)}", failures))
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(failures, ctx.request, requestId)))
    case mlf: MLFailure => ctx =>
      val requestId = getRequestId(ctx.request)
      reqId(requestId)(() => logger.debug(s"MLFailure: ${logPrint(mlf.toJson)} for ${toString(ctx.request)}", mlf))
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(mlf, ctx.request, requestId)))
    case se: ServiceException => ctx =>
      val requestId = getRequestId(ctx.request)
      if (se.status >= 405)
        ExceptionLogger.log(msg = Some(s"Error ${se.status} for ${toString(ctx.request)}"), exception = se, requestId = requestId)
      else
        reqId(requestId)(() => logger.debug(s"ServiceException: ${se.status} for ${toString(ctx.request)}: ${se.getMessage}"))
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(se, ctx.request, requestId)))
    case nie: NotImplementedError => ctx =>
      val requestId = getRequestId(ctx.request)
      ExceptionLogger.log(msg = Some(s"Not implemented for ${toString(ctx.request)}"), exception = nie, requestId = requestId)
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(nie, ctx.request, requestId)))
    case te: java.util.concurrent.TimeoutException => ctx =>
      val requestId = getRequestId(ctx.request)
      ExceptionLogger.log(msg = Some(s"Timeout for ${toString(ctx.request)}"), exception = te, requestId = requestId, callStack = true)
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(te, ctx.request, requestId)))
    case e: Exception => ctx =>
      val requestId = getRequestId(ctx.request)
      ExceptionLogger.log(msg = Some(s"Unhandled exception for ${toString(ctx.request)}"), exception = e, requestId = requestId, callStack = true)
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(e, ctx.request, requestId)))
    case t: Throwable if !t.isInstanceOf[Error] => ctx =>
      val requestId = getRequestId(ctx.request)
      ExceptionLogger.log(msg = Some(s"Unhandled throwable for ${toString(ctx.request)}"), exception = t, requestId = requestId, callStack = true)
      ctx.request.discardEntityBytes(ctx.materializer)
      ctx.complete(logResponse(ctx, getErrorResponse(t, ctx.request, requestId)))
  }

  // used by unit tests
  def getApiDocsUrl(path: String, method: String): Option[String] = {
    val configUrl = "api.doc-url"
    Try {
      val pathPattern: Regex = "/ml/v4/([^/]+)".r
      pathPattern.findFirstMatchIn(path.toLowerCase) match {
        case Some(module) =>
          Try(config.getString(s"$configUrl-$module-${method.toLowerCase}"))
            .getOrElse(Try(config.getString(s"$configUrl-$module"))
              .getOrElse(config.getString(configUrl)))
        case None =>
          ""
      }
    } match {
      case Success(value) if value.trim.nonEmpty =>
        Some(value)
      case Success(_) =>
        None
      case Failure(exception) =>
        logger.error(s"Failed to find config $configUrl for API docs: ${ServiceException.getExceptionMessage(exception)}", exception)
        None
    }
  }

  private def getApiDocsUrl(request: Option[HttpRequest]): Option[String] = {
    request match {
      case Some(req) =>
        getApiDocsUrl(req.uri.path.toString(), req.method.name())
      case None =>
        None
    }
  }
}
