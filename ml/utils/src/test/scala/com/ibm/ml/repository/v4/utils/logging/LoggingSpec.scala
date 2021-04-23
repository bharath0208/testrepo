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

import com.ibm.analytics.wml.service.utils.security.model.Identity.IAM
import com.ibm.analytics.wml.service.utils.security.model.Subject.User
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec
import org.slf4j.MDC

class LoggingSpec extends AnyWordSpec with StrictLogging {
  "Logging" when {
    "checking logging utilities" should {
      val requestId = "req-id"
      var checked = false
      "show request id" in {
        def checkReqId(): Unit = {
          assert(requestId == MDC.get(LOGGER_REQUEST_ID))
          checked = true
        }

        def abortFunc(): Unit = {
          throw new Exception("logging failed")
        }

        reqId(None)(() => logger.info("log message"))
        reqId(Some(requestId))(() => logger.info("log message"))
        reqId(None)(() => abortFunc())
        reqId(Some(requestId))(() => abortFunc())
        implicit val identity: Identity = Identity(
          Subject(
            subjectType = User,
            id = "id",
            role = Some("editor"),
            name = Some("Julian Payne")
          ),
          rawToken = "############",
          realm = IAM,
          requestId = Some(requestId)
        )
        reqId(() => logger.info("log message"))
        reqId(() => checkReqId())
        assert(checked)
      }
    }
  }
}
