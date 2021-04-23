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

import com.ibm.ml.repository.v4.tests.tags.SmokeTest
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec

@SmokeTest
class ExceptionLoggerSpec extends AnyFeatureSpec with StrictLogging {
  Feature("Logging ExceptionLogger") {
    Scenario("logging an exception") {
      ExceptionLogger.log(None, new Exception("test-error-1"), None)
      ExceptionLogger.log(Some("msg"), new Exception("test-error-2"), None)
      ExceptionLogger.log(Some("msg"), new Exception("test-error-3"), None, callStack = true)
      ExceptionLogger.log(Some("msg"), new Exception("test-error-4"), Some("req-id"))
      ExceptionLogger.log(Some("msg"), new Exception("test-error-5"), Some("req-id"), callStack = true)
    }
  }
}
