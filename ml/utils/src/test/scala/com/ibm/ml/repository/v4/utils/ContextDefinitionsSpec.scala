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

import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class ContextDefinitionsSpec extends AnyWordSpec with StrictLogging {
  "ContextDefinitions" when {
    "creating a valid actor system" in {
      val system = ContextDefinitions.createActorSystem(config, ContextDefinitions.SYSTEM_DOWNSTREAM_SERVICES)
      assert(system != null)
    }
    "creating a bad actor system" in {
      val system = Try(ContextDefinitions.createActorSystem(config, "BAD NAME"))
      assert(system.isFailure)
    }
    "creating a valid execution context" in {
      val ec = ContextDefinitions.getExecutionContext(ContextDefinitions.SYSTEM_DOWNSTREAM_SERVICES)
      assert(ec != null)
    }
    "creating a bad execution context" in {
      val ec = Try(ContextDefinitions.getExecutionContext("BAD NAME"))
      assert(ec.isFailure)
    }
  }
}
