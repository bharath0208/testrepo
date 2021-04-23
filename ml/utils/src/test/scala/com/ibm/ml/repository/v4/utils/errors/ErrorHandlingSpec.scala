/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.errors

import com.ibm.analytics.wml.utils.errors.MLFailures
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

class ErrorHandlingSpec extends AnyFeatureSpec {
  Feature("MLFailures") {
    Scenario("should return 404") {
      val jsons: Seq[String] = Seq(
        """
          |{
          | "trace": "57bec2eb823c27ffee8a84a6b76eca15",
          | "errors": [{
          |  "code": "downstream_error_cams",
          |  "message": "The service is experiencing some downstream errors, please re-try the request",
          |  "more_info": "https://cloud.ibm.com/apidocs/machine-learning"
          | }, {
          |  "code": "does_not_exist",
          |  "message": "CATSV5010E: The asset '6ed17b87-305b-4590-8251-feb40dcd212c' in catalog '62cf62ba-d3a4-426f-bdaf-55335dcbc63c' has been marked for delete."
          | }],
          | "status_code": "410"
          |}
          |""".stripMargin,
        """
          |{
          | "trace": "57bec2eb823c27ffee8a84a6b76eca15",
          | "errors": [{
          |  "code": "does_not_exist",
          |  "message": "CATSV5010E: The asset '6ed17b87-305b-4590-8251-feb40dcd212c' in catalog '62cf62ba-d3a4-426f-bdaf-55335dcbc63c' has been marked for delete."
          | }],
          | "status_code": "410"
          |}
          |""".stripMargin
      )

      val moreInfo: Option[String] = ServiceExceptionHandler.getApiDocsUrl("/ml/v4/models", "get")
      assert(moreInfo.isDefined, "Failed to get API docs url")

      for (json <- jsons) {
        val failures = JsonParser(json).convertTo[MLFailures]
        val cleaned = ServiceExceptionHandler.getErrorResponseFromFailures(failures, moreInfo)
        info(s"Cleaned failures: ${cleaned.toJson.prettyPrint} from $json")
        assert(cleaned.statusCode.isDefined)
        assert(cleaned.statusCode.get == 404)
        // we may have to remove this as sometimes the error will get removed (trimmed)
        assert(cleaned.errors.length == failures.errors.length + 1)
      }
      for (json <- jsons) {
        val failures = JsonParser(json).convertTo[MLFailures]
        val cleaned = ServiceExceptionHandler.getErrorResponseFromFailures(failures, None)
        info(s"Cleaned failures: ${cleaned.toJson.prettyPrint} from $json")
        assert(cleaned.statusCode.isDefined)
        assert(cleaned.statusCode.get == 404)
        // we may have to remove this as sometimes the error will get removed (trimmed)
        assert(cleaned.errors.length == failures.errors.length + 1)
        // just to add a jdk11 feature
        assert(!cleaned.toJson.prettyPrint.trim.isEmpty)
      }
    }
  }
}
