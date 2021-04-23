/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils.v4beta.repository

import java.net.URL

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import com.ibm.analytics.wml.api.v4.common._
import com.ibm.analytics.wml.api.v4.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4.models._
import com.ibm.analytics.wml.service.utils.security.StableServiceId
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableServiceId}
import com.ibm.analytics.wml.service.utils.security.model.Identity.IAM
import com.ibm.analytics.wml.service.utils.security.model.Subject.Service
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.ibm.analytics.wml.utils.clients.http.{CachedHttpClientBuilder, HttpClientBuilder}
import com.ibm.analytics.wml.utils.security.{IAMConfig, IAMToken, MLInstanceIdHeader, WMLUserIdHeader}
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.BeforeAndAfterAll
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class V4BetaRepositoryClientSpec extends AnyFeatureSpec with BeforeAndAfterAll with StrictLogging {
  private lazy val config: Config = ConfigFactory.load()

  private implicit val system: ActorSystem = ActorSystem("v4-beta-repository-client-spec")
  private implicit val ec: ExecutionContext = system.dispatcher
  private implicit val builder: HttpClientBuilder = CachedHttpClientBuilder()
  private implicit val iamContext: IAMContext = IAMContext()

  private val testWaitDuration: FiniteDuration = 30.seconds

  private def createClient(implicit credentialsProvider: Identity => Future[Map[String, String]]): V4BetaRepositoryClient = {
    val defaultUrl: String = "https://wml-fvt.ml.test.cloud.ibm.com"
    val serviceUrl: URL = new URL(Try(config.getString("test.v3repo.url")).getOrElse(defaultUrl))
    V4BetaRepositoryClient(
      host = serviceUrl.getHost,
      port = if (serviceUrl.getPort > 0) serviceUrl.getPort else serviceUrl.getDefaultPort
    )
  }

  Feature("V4BetaRepositoryClient") {
    /*Scenario*/ ignore("Get v3 assets using service id") {
      if (StableServiceId.verifyEnvironment.isSuccess) {
        val instanceId: String = sys.env.getOrElse("WML_INSTANCE_ID", "3c9fd487-7ab8-4eb7-a490-e0c0e9352f1d")
        val apikey: String = "dPjP-6TMHTWq3GWnQ9lnJ4q9xI9ETTzfoAhUOGmZrtN1"
        val iamConfig = IAMConfig()
        val (token, userId) = {
          val f = for {
            token <- IAMStableServiceId().createServiceIdToken()
            userToken <- IAMToken.getFromApiKey(apikey, iamConfig)
            tenant <- IAMToken.validate(userToken, iamConfig)
          } yield {
            info(s"Using service id with instance $instanceId for user ${tenant.tenantId}")
            (token, tenant.tenantId)
          }
          Await.result(f, 1.minute)
        }

        def getCredentials(identity: Identity): Future[Map[String, String]] = {
          Future.successful(
            Map(
              Authorization.name -> OAuth2BearerToken(token).value,
              MLInstanceIdHeader.name -> instanceId,
              WMLUserIdHeader.name -> userId
            )
          )
        }

        val client = createClient(getCredentials)
        val identity: Identity = Identity(
          subject = Subject(
            subjectType = Service,
            id = userId
          ),
          rawToken = token,
          realm = IAM
        )

        {
          // create a model
          val model = ModelEntity(
            modelType = "do-docplex_12.9",
            name = Some("my_model"),
            softwareSpec = Some(
              SoftwareSpecRef(id = None, name = Some("do-docplex_12.9"))
            )
          ).toJson
          val result: JsValue = Try(Await.result(client.models.createRaw(identity, model), testWaitDuration)) match {
            case Success(res) => res
            case Failure(exception) =>
              val msg = s"Failed to create model: ${exception.getMessage}"
              logger.error(msg, exception)
              info(msg)
              fail(msg, exception)
          }
          info(s"Created model: ${result.prettyPrint}")
        }
        {
          // get all the models
          val resources = Try(Await.result(client.models.list(identity), testWaitDuration)) match {
            case Success(res) => res
            case Failure(exception) =>
              val msg = s"Failed to get models: ${exception.getMessage}"
              logger.error(msg, exception)
              fail(msg, exception)
          }
          info(s"Got models: ${resources.toJson.prettyPrint}")
        }

        {
          // get all the functions
          val resources = Try(Await.result(client.functions.list(identity), testWaitDuration)) match {
            case Success(res) => res
            case Failure(exception) =>
              val msg = s"Failed to get functions: ${exception.getMessage}"
              logger.error(msg, exception)
              fail(msg, exception)
          }
          info(s"Got functions: ${resources.toJson.prettyPrint}")
        }
      } else {
        info("** warning: scenario 'Get v3 assets using service id' is disabled because service id is not configured")
      }
    }
  }
}
