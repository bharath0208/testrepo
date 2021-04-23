/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.RawHeader
import com.ibm.analytics.wml.api.v4ga.models.ModelJsonFormat._
import com.ibm.analytics.wml.repository.RepositoryClient
import com.ibm.analytics.wml.service.utils.http.WMLUserIdHeader
import com.ibm.analytics.wml.service.utils.security.StableServiceId
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableServiceId}
import com.ibm.analytics.wml.service.utils.security.model.Subject.DelegatedUser
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.clients.http.{CachedHttpClientBuilder, HttpClientBuilder, NoRetry, TokenProvider}
import com.ibm.analytics.wml.utils.errors.MLFailures
import com.ibm.ml.repository.v4.tests.tags.SmokeTest
import com.ibm.ml.repository.v4.tests.utils.V4TestServicesClient
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import java.net.URL
import java.util.Base64
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.Try

@SmokeTest
class AuthSpec extends AnyFeatureSpec with StrictLogging {
  private implicit val system: ActorSystem = ActorSystem("AuthSpec")
  private implicit val ec: ExecutionContext = system.dispatcher
  private implicit val builder: HttpClientBuilder = CachedHttpClientBuilder(allowAllCerts = true)
  private implicit val iamContext: IAMContext = IAMContext()
  private val config: Config = ConfigFactory.load()
  private val defaultUrl: String = "https://wml-fvt.ml.test.cloud.ibm.com"
  private val serviceUrl: URL = new URL(Try(config.getString(V4TestServicesClient.CONFIG_SERVICE_URL)).getOrElse(defaultUrl))

  private def logPrint(json: JsValue): String = {
    json.prettyPrint
  }

  Feature("Utils Auth") {
    Scenario("creating credentials") {
      val un = "admin"
      val pw = "asdsff"
      val secret = Base64.getEncoder.encodeToString(s"$un:$pw".getBytes("UTF-8"))
      info(s"Secret: $secret")
      val decoded = new String(Base64.getDecoder.decode(secret)).trim
      val up = decoded.split(":")
      assert(up(0) == un)
      assert(up(1) == pw)
    }
  }

  Feature("Authentication") {
    if (StableServiceId.verifyEnvironment.isSuccess) {
      Scenario("service id with user id") {
        info(s"Testing service id with user id on host $serviceUrl")

        def credentials(identity: Identity): Future[HttpCredentialsProvider] = {
          for {
            token <- IAMStableServiceId().createServiceIdToken()
          } yield {
            TokenProvider(token, extraHeaders = Vector(RawHeader(WMLUserIdHeader.name, "julianpayne@fr.ibm.com")))
          }
        }

        val client = new RepositoryClient(
          serviceUrl.getHost,
          if (serviceUrl.getPort > 0) serviceUrl.getPort else serviceUrl.getDefaultPort
        )(
          builder,
          credentials,
          system,
          NoRetry
        )

        val f = {
          for {
            models <- client.models.list(
              identity = Identity(
                subject = Subject(
                  subjectType = DelegatedUser,
                  id = "dummy_id",
                  role = None,
                  name = None,
                  serviceId = None,
                  email = None
                ),
                "raw_token",
                Identity.IAM,
                None,
                Some("service-id-with-user-id")
              ),
              version = "2020-08-01",
              spaceId = Some("dummy")
            )
          } yield {
            info(s"Created model: ${logPrint(models.toJson)}")
          }
        } recover {
          case ml: MLFailures =>
            info(s"Failure: ${logPrint(ml.toJson)}")
            assert(ml.errors.head.code == "authorization_rejected")
          case t: Throwable =>
            fail(s"Failed test: ${t.getMessage} ${t.getClass.getName}")
        }
        Await.result(f, 1.minute)
      }

      Scenario("service id without user id") {
        info(s"Testing service id without user id on host $serviceUrl")

        def credentials(identity: Identity): Future[HttpCredentialsProvider] = {
          for {
            token <- IAMStableServiceId().createServiceIdToken()
          } yield {
            TokenProvider(token)
          }
        }

        val client = new RepositoryClient(
          serviceUrl.getHost,
          if (serviceUrl.getPort > 0) serviceUrl.getPort else serviceUrl.getDefaultPort
        )(
          builder,
          credentials,
          system,
          NoRetry
        )

        val f = {
          for {
            models <- client.models.list(
              identity = Identity(
                subject = Subject(
                  subjectType = DelegatedUser,
                  id = "dummy_id",
                  role = None,
                  name = None,
                  serviceId = None,
                  email = None
                ),
                "raw_token",
                Identity.IAM,
                None,
                Some("service-id-without-user-id")
              ),
              version = "2020-08-01",
              spaceId = Some("dummy")
            )
          } yield {
            info(s"Created model: ${logPrint(models.toJson)}")
          }
        } recover {
          case ml: MLFailures =>
            info(s"Failure: ${logPrint(ml.toJson)}")
            assert(ml.errors.head.code == "authorization_rejected")
          case t: Throwable =>
            fail(s"Failed test: ${t.getMessage} ${t.getClass.getName}")
        }
        Await.result(f, 1.minute)
      }
    } else {
      logger.warn(s"*** warning service id tests disabled as environment is not configured ***")
    }
  }
}
