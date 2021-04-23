/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.logging

import akka.http.scaladsl.client.RequestBuilding
import akka.http.scaladsl.model.headers.RawHeader
import com.ibm.analytics.wml.service.utils.security.model.Identity.IAM
import com.ibm.analytics.wml.service.utils.security.model.Subject.User
import com.ibm.analytics.wml.service.utils.security.model._
import com.ibm.analytics.wml.utils.clients.http.HttpMetric
import com.ibm.ml.repository.v4.tests.tags.SmokeTest
import com.ibm.ml.repository.v4.utils.logging.AccessLogger
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec

@SmokeTest
class AccessLoggerSpec extends AnyFeatureSpec with StrictLogging {
  val now: Long = System.currentTimeMillis()
  val metrics: HttpMetric = HttpMetric(
    contextId = Some("reqId"),
    action = Some("test-action"),
    origin = Some("test-origin"),
    service = Some("test"),
    host = "test-host",
    port = 0,
    method = "GET",
    uri = "/test",
    startTime = now,
    duration = now + 1000,
    statusCode = Some(200)
  )
  Feature("Logging AccessLogger") {
    Scenario("logging request") {
      val request = RequestBuilding.Get("/ml/v4/experiments").addHeader(RawHeader("X-Forwarded-For", "192.1.1.1"))
      val identity = Identity(Subject(User, "test-id"), "raw-token", IAM, None, Some("julian"))
      AccessLogger.logAccess(request, Some(identity), 200)
      AccessLogger.logAccess(request, Some(identity), 200, requestStarted = now, requestFinished = now + 2000)
    }
    Scenario("logging downstream") {
      AccessLogger.logDownstream(metrics)
      AccessLogger.logDownstream(metrics.copy(service = None))
      AccessLogger.logDownstream(metrics.copy(statusCode = Some(500)))
      AccessLogger.logDownstream(metrics.copy(statusCode = Some(500), failure = Some(new Exception())))
      AccessLogger.logDownstream(metrics.copy(statusCode = None))
      AccessLogger.logDownstream(metrics.copy(statusCode = None, failure = Some(new Exception())))
    }
  }
}
