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

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.utils.HeartbeatJsonFormat.heartbeatResponseFormat
import com.ibm.analytics.wml.utils.{HeartbeatResponse, ServiceDetails, ServiceNames}
import com.ibm.ml.repository.v4.service.TestClient
import com.ibm.ml.repository.v4.service.TestClient._
import com.ibm.ml.repository.v4.tests.tags.SmokeTest
import com.ibm.ml.repository.v4.utils._
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.BeforeAndAfterAll
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import scala.util.Try

@SmokeTest
class HeartbeatAndVersionSpec extends AnyFeatureSpec with BeforeAndAfterAll with StrictLogging {
  private def PATH_GET_VERSION: String = "/ml/wml_services/version"

  private def PATH_GET_HEARTBEAT: String = "/ml/wml_services/ml-repository/heartbeat"

  private def PATH_GET_TEST: String = "/ml/wml_services/ml-repository/test"

  private lazy val config: Config = ConfigFactory.load()

  private lazy val SCHEME: String = config.getString("tests.scheme")
  private lazy val HOST: String = config.getString("tests.host")
  private lazy val PORT: Int = config.getInt("tests.port")

  private var client: Option[TestClient] = None

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    client = Some(TestClient())
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    client.foreach(c => Try(c.close()))
  }

  private def getClient: TestClient = {
    client match {
      case Some(c) => c
      case None => fail("No TestClient created")
    }
  }

  private def absoluteUrl(url: String): String = {
    if (url.toLowerCase.startsWith(s"$SCHEME".toLowerCase))
      url
    else
      s"$SCHEME://$HOST:$PORT$url"
  }

  // test service version
  Feature("API Version") {
    Scenario("checking version") {
      val uri = absoluteUrl(PATH_GET_VERSION)
      info(s"Checking version on $uri")
      val response = getClient.get(uri, Some("check-get-version"))
      val entity = getAndLogResponse(response)
      assert(getStatus(response) == StatusCodes.OK.intValue)
      assert(entity.isDefined)
      val sd = entity.get.convertTo[ServiceDetails]
      info(s"Service details: ${logPrint(sd.toJson)}")
      assert(sd.platform != null)
    }
  }

  // test heart beat
  Feature("API Heartbeat") {
    Scenario("checking heartbeat") {
      var tests = 0
      val uri = absoluteUrl(PATH_GET_HEARTBEAT)
      info(s"Checking heartbeat on $uri")
      val response = getClient.get(uri, Some("check-get-heartbeat"))
      val entity = getAndLogResponse(response)
      assert(getStatus(response) == StatusCodes.OK.intValue)
      assert(entity.isDefined)
      val hb = entity.get.convertTo[HeartbeatResponse]
      info(s"Heartbeat: ${logPrint(hb.toJson)}")
      assert(hb.serviceName.contains(ServiceNames.wmlRepositoryV4))
      for (header <- response.getAllHeaders) {
        if (header.getName == "Server") {
          tests += 1
          if (header.getValue != "WML V4 Repository 4.0.0")
            logger.warn(s"Server header overridden: ${header.getValue}")
          //assert(header.value() == "WML V4 Repository 4.0.0")
        }
      }
      assert(tests >= 1)
      info("Heartbeat get checking done...")
    }
    Scenario("post should fail with 405") {
      val uri = absoluteUrl(PATH_GET_HEARTBEAT)
      info(s"Checking heartbeat on $uri")
      val response = getClient.post(uri, jsonEntity(Some("{}")), Some("check-post-heartbeat"), Some(StatusCodes.MethodNotAllowed.intValue))
      val _ = getAndLogResponse(response)
      assert(getStatus(response) == StatusCodes.MethodNotAllowed.intValue)
      info("Heartbeat post checking done...")
    }
  }

  Feature("API Tests") {
    Scenario("checking timeout test") {
      val uri = absoluteUrl(s"$PATH_GET_TEST?timeout=100")
      info(s"Checking test on $uri")
      val response = getClient.post(uri, jsonEntity(Some("{}")), Some("check-post-test"), Some(StatusCodes.OK.intValue))
      val entity = getAndLogResponse(response)
      assert(getStatus(response) == StatusCodes.OK.intValue)
      assert(entity.isDefined)
      info("Test API get checking done...")
    }
  }
}
