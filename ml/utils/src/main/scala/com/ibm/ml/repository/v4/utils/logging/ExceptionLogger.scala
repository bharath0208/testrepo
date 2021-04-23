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

import com.ibm.ml.repository.v4.utils.ServiceException
import com.typesafe.scalalogging.Logger
import org.slf4j.{LoggerFactory, MDC}

import scala.util.Try

object ExceptionLogger {
  // this is the MDC parameter for logging the request ID
  val LOGGER_REQUEST_ID = "Request-ID"
  private val exceptionLogger: Logger = Logger(LoggerFactory.getLogger("exception-logger"))

  // we must not throw an exception here
  def log(msg: Option[String],
          exception: Throwable,
          requestId: Option[String],
          callStack: Boolean = false): Unit = {
    val reqId = requestId match {
      case Some(r) => r
      case None => "-"
    }
    val message = msg match {
      case Some(m) =>
        s"$m: ${ServiceException.getExceptionMessage(exception)}"
      case None =>
        ServiceException.getExceptionMessage(exception)
    }
    try {
      Try(MDC.put(LOGGER_REQUEST_ID, reqId))
      if (callStack || (exception.isInstanceOf[ServiceException] &&
        (exception.asInstanceOf[ServiceException].status == 500)))
        Try(exceptionLogger.warn(message, exception))
      else
        Try(exceptionLogger.info(message))
    }
    finally {
      Try(MDC.remove(LOGGER_REQUEST_ID))
    }
  }
}
