/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.logging

import akka.http.scaladsl.model.{HttpRequest, Uri}
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.ServiceNames
import com.ibm.analytics.wml.utils.clients.http.{CallerIP, HttpMetric}
import com.ibm.analytics.wml.utils.clients.reporting.ReportMetrics
import com.ibm.ml.repository.v4.utils._
import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success, Try}

object AccessLogger {
  private val accessLogger: Logger = Logger(LoggerFactory.getLogger("http-access"))

  private val serviceNameLength: Int = 8
  private val requestIdLength: Int = 1

  private def getCallerIp(request: HttpRequest): String = {
    CallerIP.getCallerIP(request) match {
      case Some(ip) => ip
      case None => ""
    }
  }

  private def getCaller(identity: Option[Identity]): String = {
    identity match {
      case Some(id) => s" ${id.subject.id}"
      case _ => ""
    }
  }

  private def getUser(identity: Option[Identity]): String = {
    val userId = identity match {
      case Some(ident) => Some(ident.subject.id)
      case None => None
    }
    optional(userId, prefix = " for user ")
  }

  private def getDate(now: Long): String = {
    s"[${formatAsDate(now)}]"
  }

  private def optional(opt: Option[Any],
                       defaultValue: String = "",
                       quote: Boolean = false,
                       prefix: String = ""): String = {
    opt match {
      case Some(v) if v.toString != "None" => if (quote) s"$prefix'$v'" else s"$prefix$v"
      case _ => defaultValue
    }
  }

  private def pad(s: String, len: Int, truncate: Boolean = false): String = {
    val sb = new StringBuilder()
    sb.append(s)
    while (sb.length < len)
      sb.append(' ')
    if (truncate)
      sb.toString.substring(0, Math.min(sb.length, len))
    else
      sb.toString
  }

  private def getUri(uri: Uri): String = {
    uri.toRelative.toString
  }

  private def getUri(uri: String): String = {
    Try {
      Uri(uri)
    } match {
      case Success(url) =>
        getUri(url)
      case Failure(_) =>
        uri
    }
  }

  private def getError(metrics: HttpMetric): String = {
    metrics.statusCode match {
      case Some(status) if status > 404 =>
        metrics.failure match {
          case Some(t) =>
            t.getMessage
          case None =>
            ""
        }
      case _ =>
        ""
    }
  }

  private val OK: String = "OK"
  private val KO: String = "KO"
  private val UNKNOWN: String = "--"

  private def getOK(metrics: HttpMetric): String = {
    metrics.statusCode match {
      case Some(status) if status >= 400 => KO
      case Some(_) => OK
      case None =>
        metrics.failure match {
          case Some(_) => KO
          case None =>
            // if we get here we don't know
            UNKNOWN
        }
    }
  }

  private def getOK(status: Int): String = {
    if (status >= 400)
      KO
    else
      OK
  }

  def logAccess(request: HttpRequest,
                identity: Option[Identity],
                status: Int,
                requestStarted: Long = -1,
                requestFinished: Long = System.currentTimeMillis()): Unit = {
    // we must not throw an exception here
    Try {
      val caller = pad(s"service ${getCallerIp(request)}${getCaller(identity)}", serviceNameLength, truncate = true)
      val requestId = pad(optional(getRequestId(request)), requestIdLength)
      if (requestStarted > 0)
        accessLogger.info(s"${getOK(status)} $caller ${getDate(requestFinished)} [$requestId] ${request.method.value} ${getUri(request.uri)} $status ${requestFinished - requestStarted}${getUser(identity)}")
      else
        accessLogger.info(s"${getOK(status)} $caller ${getDate(requestFinished)} [$requestId] ${request.method.value} ${getUri(request.uri)} $status${getUser(identity)}")
    }
  }

  private def logWithTypeAndError(identity: Option[Identity],
                                  label: String,
                                  err: HttpMetric => String)
                                 (metrics: HttpMetric): Unit = {
    // we must not throw an exception here
    Try {
      val status = optional(metrics.statusCode, defaultValue = "-")
      val action = optional(metrics.action)
      val service = optional(metrics.service, quote = true)
      val downstream = pad(label, serviceNameLength, truncate = true)
      val requestId = pad(optional(metrics.contextId), requestIdLength)
      val error = err(metrics)
      accessLogger.info(s"${getOK(metrics)} $downstream ${getDate(metrics.startTime + metrics.duration)} [$requestId] ${metrics.method} ${getUri(metrics.uri)} $status ${metrics.duration} $service $action $error${getUser(identity)}")
    }
  }

  def logDownstream(metrics: HttpMetric): Unit = {
    val label: String = metrics.service match {
      case Some(service) => service
      case _ => "downstream"
    }
    logWithTypeAndError(None, label, getError)(metrics)
  }

  def logAuthentication(metrics: HttpMetric): Unit = {
    logWithTypeAndError(None, "authentication", getError)(metrics)
  }

  def logDataAccess(identity: Identity,
                    metrics: HttpMetric,
                    downstream: String): Unit = {
    def oneOf(value: String, values: String*): Boolean = {
      for (v <- values) {
        if (v.equalsIgnoreCase(value))
          return true
      }
      false
    }
    metrics.action match {
      case Some(action) if oneOf(action, "list", "create", "read", "update", "delete") =>
      // ok
      case Some(action) =>
        throw new Exception(s"Unknown data access action '$action' for data access")
      case None =>
        throw new Exception(s"No data access action for data access")
    }
    downstream.toLowerCase.trim match {
      case "s3" =>
        logWithTypeAndError(Some(identity), downstream, getError)(metrics)
      case "fs" =>
        logWithTypeAndError(Some(identity), downstream, getError)(metrics)
      case "connector" =>
        logWithTypeAndError(Some(identity), downstream, getError)(metrics)
      case "cams" =>
        logWithTypeAndError(Some(identity), downstream, getError)(metrics)
      case unknown =>
        throw new Exception(s"Unknown downstream '$unknown' for data access")
    }
  }

  /**
   * The service logs all data access and data manipulation events.
   * We interpret this to mean that any data access (so reading from COS etc)
   * should be logged with the identity.subject.id and the data operation -
   * accessing repo assets will be logged by the repo service,
   * in addition all services must correctly handle the request id and print it in all logs.
   *
   * @param action The action, one of "list", "create", "read", "update", "delete".
   * @param service The service name, one of ServiceNames.wmlRepositoryV4 or ServiceNames.wmlRepositoryMigrationV4.
   * @param downstream The downstream data source, something like "S3" or "FS".
   * @param host The downstream host name.
   * @param port The downstream port number.
   * @param method The HTTP method.
   * @param uri The downstream uri.
   * @param startTime The start time of the data access.
   * @param duration: The duration of the data access.
   * @param status The status code of the data access.
   * @param requestId The request id.
   * @param failure The failure if known.
   * @param callerIP The client IP if known.
   */
  def logDataAccess(identity: Identity,
                    action: String, // one of "list", "create", "read", "update", "delete"
                    service: String = ServiceNames.wmlRepositoryV4, // or ServiceNames.wmlRepositoryMigrationV4
                    downstream: String, // something like "S3" or "FS"
                    host: String,
                    port: Int,
                    method: String,
                    uri: String,
                    startTime: Long,
                    duration: Long = -1,
                    status: Option[Int] = None,
                    requestId: Option[String],
                    failure: Option[Throwable] = None,
                    callerIP: Option[String]): Unit = {
    val metrics = HttpMetric(
      contextId = requestId,
      action = Some(action),
      origin = None,
      service = Some(service),
      host = host,
      port = port,
      method = method,
      uri = uri,
      startTime = startTime,
      duration = duration,
      statusCode = status,
      failure = failure,
      callerIP = callerIP
    )
    logDataAccess(identity, metrics, downstream)
  }
}
