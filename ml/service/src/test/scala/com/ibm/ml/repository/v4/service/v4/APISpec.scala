/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.v4

import akka.http.scaladsl.client.RequestBuilding
import akka.http.scaladsl.model.headers.RawHeader
import com.ibm.ml.repository.v4.tests.tags.SmokeTest
import com.ibm.ml.repository.v4.utils.{APIUtils, HEADER_REQUEST_ID}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec

@SmokeTest
class APISpec extends AnyFeatureSpec with StrictLogging with APIUtils {
  Feature("API Utils") {
    Scenario("getting requestId headers") {
      val request1 = RequestBuilding.Get("/ml/v4/experiments").addHeader(RawHeader(HEADER_REQUEST_ID, "julian1"))
      val headers1 = getRequestIdHeadersForResponse(request1)
      assert(headers1.nonEmpty)
      assert(headers1.head.value() == "julian1")
      val request2 = RequestBuilding.Get("/ml/v4/experiments")
      val headers2 = getRequestIdHeadersForResponse(request2)
      assert(headers2.isEmpty)
    }
    Scenario("getting response headers") {
      val request1 = RequestBuilding.Get("/ml/v4/experiments")
      val headers1 = getResponseHeaders(request1)
      assert(headers1.nonEmpty)
      val request2 = RequestBuilding.Get("/ml/v4/experiments").addHeader(RawHeader("X-WML-TEST", "julian2"))
      val headers2 = getResponseHeaders(request2)
      assert(headers2.nonEmpty)
      assert(headers2.size == (headers1.size + 1))
      assert(headers2.contains(RawHeader("X-WML-TEST", "julian2")))
    }
  }
}
