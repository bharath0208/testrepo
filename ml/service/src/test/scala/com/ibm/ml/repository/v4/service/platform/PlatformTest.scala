/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.platform

import akka.actor.ActorSystem
import akka.http.scaladsl.model.HttpResponse
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.TypesResponse
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecJsonFormat._
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecifications
import com.ibm.analytics.spaces.api.v2.SpacesJsonFormat._
import com.ibm.analytics.spaces.api.v2._
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.clients.ClientUtils
import com.ibm.analytics.wml.service.utils.cloudinstance.InstanceClient
import com.ibm.analytics.wml.service.utils.security.StableServiceId
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableServiceId}
import com.ibm.analytics.wml.service.utils.security.model.Identity.IAM
import com.ibm.analytics.wml.service.utils.security.model.Subject.User
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.ibm.analytics.wml.utils.clients.http._
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.containers.{Container, Space}
import com.ibm.ml.repository.v4.tests.tags.PlatformAPI
import com.ibm.ml.repository.v4.utils._
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.BeforeAndAfterAll
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import java.util.UUID
import scala.collection.immutable.HashSet
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

@PlatformAPI
class PlatformTest extends AnyFeatureSpec with BeforeAndAfterAll with StrictLogging {
  private implicit val system: ActorSystem = ActorSystem("platform-test-actor-system")
  private implicit val ec: ExecutionContext = system.dispatcher
  private implicit val clientBuilder: HttpClientBuilder = CachedHttpClientBuilder(
    service = Some("platform-tests"),
    allowAllCerts = true,
    reporting = reporter
  )
  private implicit val credentialsProvider: Identity => Future[HttpCredentialsProvider] = ClientUtils.getCredentialsProvider
  private implicit val retryPolicy: RetryPolicy = RetryOnCodesWithDelay(3, 500, HashSet(404))

  private var serviceIdHeaders: Try[HttpCredentialsProvider] = Failure(new Exception("Service headers uninitialized"))

  // used just for the requestId
  private val dummyIdentity = Identity(
    Subject(User, "julianpayne@fr.ibm.com"),
    "raw_token",
    IAM
  )
  private var container: Option[Container] = None
  private val waitDuration: FiniteDuration = 1.minute

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    assert(platformHost.isSuccess)
    logger.info("Creating space for platform tests using the WML service id")
    StableServiceId.verifyEnvironment match {
      case Success(_) =>
        Try {
          Await.result(IAMStableServiceId()(IAMContext()).createServiceIdToken(), waitDuration)
        } match {
          case Success(token) =>
            logger.info(s"Created service id token $token")
            serviceIdHeaders = Success(TokenProvider(token))
          case Failure(exception) =>
            logger.info(s"Failed to create the service id token: ${exception.getMessage}")
            serviceIdHeaders = Failure(exception)
        }
      case Failure(exception) =>
        logger.info(s"Service id not configured: ${exception.getMessage}")
        serviceIdHeaders = Failure(exception)
    }
    if (serviceIdHeaders.isSuccess) {
      container = createSpace(Some("platform-tests-create-space")) match {
        case Success(container) =>
          logger.info(s"Created $container for platform tests")
          Some(container)
        case Failure(exception) =>
          fail(s"Failed to create space: ${exception.getMessage}", exception)
      }
    } else {
      logger.info("Failed to create the service id")
    }
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    if (container.nonEmpty)
      deleteSpace(container.get.id, Some("platform-tests-delete-space"))
  }

  private def getRequestIdHeaders(identity: Identity): Future[Map[String, String]] = {
    val headers: Map[String, String] = if (identity.requestId.isDefined) Map(HEADER_GLOBAL_TRANSACTION_ID -> identity.requestId.get) else Map()
    info(s"Using headers $headers")
    Future.successful(headers)
  }

  private def reporter(metrics: HttpMetric): Unit = {
    info(s"${metrics.method} ${metrics.host}:${metrics.port}${metrics.uri} ${if (metrics.statusCode.isDefined) metrics.statusCode.get.toString else "-"}")
  }

  private def checkResponse(response: HttpResponse): Future[Unit] = {
    if (response.status.isSuccess()) {
      Future.successful(())
    } else {
      {
        for {
          reply <- Unmarshal(response.entity).to[String]
        } yield {
          throw new Exception(s"Request failed with ${response.status} and response $reply")
        }
      } recoverWith {
        case e: Throwable =>
          Future.failed(e)
      }
    }
  }

  private def getSpaceStatus(client: AkkaHttpClient,
                             spaceId: String,
                             requestId: Option[String]): Future[SpaceEntity] = {
    for {
      headers <- getRequestIdHeaders(dummyIdentity.copy(requestId = requestId))
      response <- client.get(
        uri = s"/v2/spaces/$spaceId",
        action = Some("get-space"),
        headers = headers
      )(None, retryPolicy)
      _ <- checkResponse(response)
      json <- client.toJson(response)
      status <- Future.fromTry(Try(json.convertTo[SpaceEntity]))
    } yield {
      status
    }
  }

  private case class StateException(state: SpaceStateType) extends Exception(state.name)

  private def waitForSpaceCreation(client: AkkaHttpClient,
                                   spaceId: String,
                                   requestId: Option[String]): Future[String] = {
    for {
      status <- getSpaceStatus(client, spaceId, requestId)
      spaceId <- {
        status.status.state match {
          case ActiveState =>
            logger.info(s"Space $spaceId created: ${logPrint(status.toJson)}")
            Future.successful(spaceId)
          case PreparingState =>
            val delay = 500
            val duration = Duration(delay + Random.nextInt(Random.nextInt(delay)), MILLISECONDS)
            logger.info(s"Preparing space $spaceId - trying again in $duration")
            val promise = Promise[String]()
            system.scheduler.scheduleOnce(duration) {
              promise.completeWith(waitForSpaceCreation(client, spaceId, requestId))
            }
            promise.future
          case _ =>
            val msg = s"Space creation failed: ${logPrint(status.toJson)}"
            logger.error(msg)
            Future.failed(new Exception(msg))
        }
      }
    } yield {
      spaceId
    }
  }

  private def createSpace(requestId: Option[String]): Try[Container] = {
    val storage = Some(
      SpaceStorageRequest(
        sys.env("WML_COS_CRN"),
        None
      )
    )
    val req = SpaceEntityRequest(
      name = s"test-space-platform-test-${UUID.randomUUID().toString}",
      description = None,
      storage = storage,
      compute = None
    )
    val client = clientBuilder.get(platformHost.get, platformPort.get, HttpClientBuilder.Keys.SPACES_CLIENT)
    val spaceId: Future[String] = for {
      headers <- getRequestIdHeaders(dummyIdentity.copy(requestId = requestId))
      response <- client.postJson(
        uri = "/v2/spaces",
        action = Some("create-space"),
        headers = headers,
        body = req.toJson
      )(None, retryPolicy)
      _ <- checkResponse(response)
      json <- client.toJson(response)
      spaceId <- {
        info(s"Create space response: ${logPrint(json)}")
        val spaceId = json.asJsObject.fields("metadata").asJsObject.fields("id").asInstanceOf[JsString].value
        waitForSpaceCreation(client, spaceId, requestId)
      }
    } yield {
      spaceId
    }
    Try(Await.result(spaceId, waitDuration)) match {
      case Success(id) =>
        val cont = Space(id)
        info(s"Created $cont")
        Success(cont)
      case Failure(t) =>
        Failure(t)
    }
  }

  private def deleteSpace(spaceId: String,
                          requestId: Option[String]): Unit = {
    val client = clientBuilder.get(platformHost.get, platformPort.get, HttpClientBuilder.Keys.SPACES_CLIENT)
    val deleteIt: Future[Unit] = for {
      headers <- getRequestIdHeaders(dummyIdentity.copy(requestId = requestId))
      response <- client.delete(
        uri = s"/v2/spaces/$spaceId",
        action = Some("delete-space"),
        headers = headers
      )(None, retryPolicy)
      _ <- checkResponse(response)
    } yield {
    }
    Try(Await.result(deleteIt, waitDuration)) match {
      case Success(_) => logger.info(s"Deleted space $spaceId")
      case Failure(exception) => logger.error(s"Failed to delete $spaceId: ${exception.getMessage}", exception)
    }
  }

  if (serviceIdHeaders.isSuccess && container.isDefined) {
    Feature("Platform Spaces") {
      Scenario("Test WML service id") {
        val client = clientBuilder.get(platformHost.get, platformPort.get, HttpClientBuilder.Keys.SPACES_CLIENT)
        val json: Future[JsValue] = for {
          headers <- getRequestIdHeaders(dummyIdentity.copy(requestId = Some("get-space")))
          response <- client.get(
            uri = s"/v2/spaces/${container.get.id}",
            action = Some("get-space"),
            headers = headers
          )(None, retryPolicy)
          _ <- checkResponse(response)
          json <- client.toJson(response)
        } yield {
          json
        }
        Try(Await.result(json, waitDuration)) match {
          case Success(js) =>
            info(s"Found space: ${logPrint(js)}")
          case Failure(t) =>
            fail(s"Failed to get space: ${t.getMessage}", t)
        }
      }
    }
    Feature("Platform CAMS") {
      Scenario("Test WML service id") {
        val camsClient = new CAMSClient(
          camsHost = platformHost.get,
          camsPort = platformPort.get
        )
        val types: TypesResponse = Await.result(camsClient.getAllAssetTypes(dummyIdentity, container.get, Some("bmcos_object_storage")), waitDuration)
        info(s"Got CAMS asset types:\n${logPrint(types.toJson)}")
      }
    }
    Feature("Platform Environments") {
      Scenario("Test WML service id") {
        val environmentsClient = new EnvironmentsClient(
          envsHost = platformHost.get,
          envsPort = platformPort.get
        )
        val specs: SoftwareSpecifications = Await.result(environmentsClient.getSoftwareSpecsResources(dummyIdentity, None, container), waitDuration)
        info(s"Got software specs:\n${logPrint(specs.toJson)}")
      }
    }
    Feature("Platform Projects") {
      Scenario("Test WML service id") {

      }
    }

    Feature("Instances") {
      /*Scenario*/ ignore("Get V4 instance") {
        import com.ibm.analytics.wml.service.utils.security.model.IdentityFormat._
        val builder = CachedHttpClientBuilder()
        val instance_id = "a150e857-4005-45bd-95df-55b170f4e41c"
        val instanceClient = InstanceClient(builder)
        val token = serviceIdHeaders.get.getHttpCredentials.token()
        val instance = Await.result(instanceClient.getInstance(instanceId = instance_id, token = token, None, None), 1.minute)
        info(s"V4 instance: ${instance.toJson.prettyPrint}")
      }
    }
  } else {
    if (serviceIdHeaders.isFailure)
      info(s"*** Platform tests are disabled because service id is not enabled ${serviceIdHeaders.failed.get.getMessage} ***")
    else if (container.isEmpty)
      info(s"*** Platform tests are disabled because failed to create space (with service headers ${serviceIdHeaders.get}) ***")
    else
      info(s"*** Platform tests are disabled for unknown reason ***")
  }
}
