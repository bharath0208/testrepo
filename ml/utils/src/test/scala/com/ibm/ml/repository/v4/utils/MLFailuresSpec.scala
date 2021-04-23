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

import com.ibm.analytics.wml.utils.errors.{MLFailure, MLFailures}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec

class MLFailuresSpec extends AnyWordSpec with StrictLogging {
  "MLFailures" when {
    "creating an error response" in {
      val reqId = "julian"

      {
        val error = MLFailures(
          errors = Seq(MLFailure("code", "message")),
          trace = reqId,
          statusCode = 404
        )
        assert(error.statusCode.isDefined)
        assert(error.statusCode.get == 404)
        assert(error.trace == reqId)
        assert(error.errors.size == 1)
      }
      {
        val error = MLFailures(
          errors = Seq(MLFailure("code", "message"), MLFailure("code", "message")),
          trace = reqId,
          statusCode = 404
        )
        assert(error.statusCode.isDefined)
        assert(error.statusCode.get == 404)
        assert(error.trace == reqId)
        assert(error.errors.size == 2)
      }
    }
  }
}
