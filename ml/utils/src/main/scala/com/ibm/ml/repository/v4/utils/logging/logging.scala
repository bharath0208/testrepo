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

import com.ibm.analytics.wml.service.utils.security.model.Identity
import org.slf4j.MDC

import scala.util.Try

package object logging {
  // this is the MDC parameter for logging the request ID
  val LOGGER_REQUEST_ID = "Request-ID"

  // no exceptions must be thrown - hence the Try
  private def log(requestId: Option[String], f: () => Unit): Unit = {
    requestId match {
      case Some(requestId) =>
        try {
          Try(MDC.put(LOGGER_REQUEST_ID, requestId))
          Try(f())
        }
        finally {
          Try(MDC.remove(LOGGER_REQUEST_ID))
        }
      case None =>
        Try(f())
    }
  }

  def reqId(requestId: Option[String])(f: () => Unit): Unit = {
    log(requestId, f)
  }

  def reqId(f: () => Unit)(implicit identity: Identity): Unit = {
    reqId(identity.requestId)(f)
  }
}
