/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils

import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.RequestContext
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.security.ConfigDecrypter
import com.ibm.ml.repository.v4.utils.errors.ServiceExceptionHandler
import com.ibm.ml.repository.v4.utils.logging.reqId
import com.typesafe.scalalogging.StrictLogging
import spray.json.JsValue

import scala.collection.{immutable, mutable}
import scala.util.Try

trait APIUtils extends ConfigDecrypter with StrictLogging {
  val QUERY_REV: String = "rev"
  val QUERY_SPACE_ID: String = "space_id"
  val QUERY_PROJECT_ID: String = "project_id"
  val QUERY_VERSION: String = "version"
  val QUERY_CONTENT_FORMAT: String = "content_format"
  val QUERY_PIPELINE_NODE_ID = "pipeline_node_id"
  val QUERY_DEPLOYMENT_ID = "deployment_id"
  val QUERY_NAME = "name"
  val QUERY_TAG_VALUE = "tag.value"
  val QUERY_START: String = "start"
  val QUERY_LIMIT: String = "limit"
  val QUERY_SEARCH: String = "search"
  val QUERY_LIMIT_DEFAULT: Int = 100
  val QUERY_PURGE_ON_DELETE: String = "purge_on_delete"
  val QUERY_PURGE_ON_DELETE_DEFAULT: Boolean = true
  val QUERY_MODEL_TYPE = "model_type"
  val QUERY_FRAMEWORK = "framework"
  val QUERY_SOFTWARE_SPEC = "software_spec"

  protected def rejectBlank(identity: Identity,
                            value: String,
                            name: String): String = {
    if (value.trim.isEmpty) {
      reqId(identity.requestId)(() => logger.info(s"Rejecting blank query parameter '$name'"))
      throw ServiceException(StatusCodes.BadRequest, InvalidQueryParameterMessage(parameter = name, value = value, reason = Some("value is empty")))
    }
    value
  }

  private val ignoreBlankQueryParameters: Boolean = {
    Try(config.getBoolean("service.ml-repository.ignore-blank-query-parameters")).getOrElse(false)
  }

  protected def ignoreBlank(identity: Identity,
                            value: Option[String],
                            name: String): Option[String] = {
    if (ignoreBlankQueryParameters) {
      value match {
        case Some(v) if v.trim.isEmpty =>
          reqId(identity.requestId)(() => logger.info(s"Ignoring blank query parameter '$name'"))
          None
        case v =>
          v
      }
    } else
      value
  }

  /**
   * The header(s) that contain the request id.
   */
  def getRequestIdHeadersForResponse(request: HttpRequest): immutable.Seq[HttpHeader] = {
    getRequestId(request) match {
      case Some(requestId) =>
        Vector(RawHeader(HEADER_GLOBAL_TRANSACTION_ID, requestId))
      case None =>
        Nil
    }
  }

  /**
   * The return headers are specific headers that are copied
   * to the response when no caching is required.
   */
  def getNoCacheResponseHeaders: immutable.Seq[HttpHeader] = {
    // we add all the cache headers
    NO_CACHE_HEADERS
  }

  /**
   * The return headers are specific headers that are copied from the request to the response.
   */
  def getResponseHeaders(request: HttpRequest): immutable.Seq[HttpHeader] = {
    val headers = mutable.Buffer.empty[HttpHeader]
    for (header <- request.headers) {
      for (copy <- COPY_HEADERS) {
        if ((copy.endsWith("*") && header.name.toLowerCase.startsWith(copy.substring(0, copy.length - 1).toLowerCase))
          || copy.equalsIgnoreCase(header.name())) {
          headers += header
        }
      }
    }
    // we add the security headers to all calls
    SECURITY_HEADERS ++ headers.toVector
  }

  protected val componentName: String = {
    val name = getClass.getSimpleName
    if (name.endsWith("$"))
      name.substring(0, name.length - 1)
    else
      name
  }

  protected def logMethod(logger: com.typesafe.scalalogging.Logger, msg: String, identity: Identity): Unit = {
    // must not throw an exception
    Try(reqId(identity.requestId)(() => logger.debug(msg)))
  }

  protected def enter(logger: com.typesafe.scalalogging.Logger, ctx: RequestContext, identity: Identity): Unit = {
    logMethod(logger, s"$componentName: user ${identity.subject.id} entering ${ctx.request.method.value} ${ctx.request.uri}", identity)
  }

  protected def exit(logger: com.typesafe.scalalogging.Logger, ctx: RequestContext, identity: Identity): Unit = {
    logMethod(logger, s"$componentName: user ${identity.subject.id} exiting ${ctx.request.method.value} ${ctx.request.uri}", identity)
  }

  protected def timeoutResponse(request: HttpRequest, identity: Identity, duration: => Long): HttpResponse = {
    ServiceExceptionHandler.getTimeoutResponse(identity.requestId, Some(request), Some(duration))
  }

  protected def getJsonEntity(json: JsValue): ResponseEntity = {
    HttpEntity(MediaTypes.`application/json`, json.prettyPrint)
  }

  protected def checkSpaceOrProjectId(spaceId: Option[String],
                                      projectId: Option[String],
                                      spaceOnly: Boolean = false): (Option[String], Option[String]) = {
    if (spaceOnly) {
      if (spaceId.getOrElse("").trim.isEmpty)
        throw ServiceException(StatusCodes.BadRequest, MissingQueryParameterMessage(QUERY_SPACE_ID))
    } else {
      if (spaceId.getOrElse("").trim.isEmpty && projectId.getOrElse("").trim.isEmpty)
        throw ServiceException(StatusCodes.BadRequest, MissingOneOfQueryParametersMessage(QUERY_SPACE_ID, QUERY_PROJECT_ID))
      if (spaceId.nonEmpty && projectId.nonEmpty)
        throw ServiceException(StatusCodes.BadRequest, MoreThanOneOfQueryParametersMessage(QUERY_SPACE_ID, QUERY_PROJECT_ID))
    }
    (spaceId, projectId)
  }

  protected def checkContentTypeHeader(contentType: Option[String]): String = {
    contentType match {
      case Some(cType) if cType.trim.nonEmpty => cType
      case _ =>
        if(enableContentCheck)
          throw ServiceException(StatusCodes.BadRequest, ContentTypeMissingMessage("Content-Type header missing in the request"))
        else "application/octet-stream"
    }
  }
}
