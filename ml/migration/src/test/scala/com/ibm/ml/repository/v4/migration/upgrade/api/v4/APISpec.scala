/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.api.v4

import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.models.MigrationResources
import com.ibm.ml.repository.v4.migration.utils.{GetIamToken, TestClient}
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.BeforeAndAfterAll
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import scala.util.Try

class APISpec extends AnyFeatureSpec with BeforeAndAfterAll with StrictLogging {
  private lazy val config: Config = ConfigFactory.load()

  private lazy val SCHEME: String = config.getString("tests.scheme")
  private lazy val HOST: String = config.getString("tests.host")
  private lazy val PORT: Int = config.getInt("tests.port")

  private var client: Option[TestClient] = None
  private var authenticationHeaders: Map[String, String] = Map()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    client = Some(TestClient())
    // set up the authentication headers
    authenticationHeaders = GetIamToken(client.get).getAuthenticationHeaders
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

  def logPrint(json: JsValue): String = {
    json.prettyPrint
  }

  def log(msg: String): Unit = {
    info(msg)
    logger.info(msg)
  }

  Feature("API Tests") {
    Scenario("calling with no authentication") {
      val response = getClient.get(
        absoluteUrl(url = "/ml/v4/repository"),
        requestId = Some("get-all-migration-jobs-no-authentication"),
        expectedStatus = Some(401)
      )
      val entity = TestClient.getEntity(response)
      log(s"Get all migration jobs with no authentication response: $entity")
      assert(entity.isDefined)
      assert(entity.get.contains("authorization_rejected"))
    }
    Scenario("getting all migration jobs with no space_id or project_id") {
      val response = getClient.get(
        absoluteUrl(url = "/ml/v4/repository"),
        requestId = Some("get-all-migration-jobs"),
        expectedStatus = Seq(200, 400),
        authenticationHeaders = authenticationHeaders
      )
      assert(response.getStatusLine.getStatusCode == 400)
      val entity = TestClient.getEntity(response)
      log(s"Get all migration jobs response: $entity")
      assert(entity.isDefined)
      if (response.getStatusLine.getStatusCode == 200) {
        val migrationJobs = JsonParser(entity.get).convertTo[MigrationResources]
        log(s"Found the following migration jobs: ${logPrint(migrationJobs.toJson)}")
      }
    }
  }
}
