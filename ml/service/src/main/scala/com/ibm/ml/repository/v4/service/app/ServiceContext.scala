/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken, RawHeader}
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.containers.ContainersClient
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.http.WMLUserIdHeader
import com.ibm.analytics.wml.service.utils.security.iam.IAMStableServiceId
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.ibm.analytics.wml.service.utils.security.{AuthContext, Environment}
import com.ibm.analytics.wml.utils._
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.clients.http.{HttpClientBuilder, RetryFailure, TokenProvider}
import com.ibm.ml.repository.v4.service.reporting.ReportingAgent
import com.ibm.ml.repository.v4.utils.cams.{AssetStatus, GlobalAssetTypesCreator}
import com.ibm.ml.repository.v4.utils.logging.{AccessLogger, reqId}
import com.ibm.ml.repository.v4.utils.{ContextUtils, _}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import java.util.concurrent.atomic.AtomicBoolean
import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

// this is a trait but it is assumed that there will
// only be a single instance of this in the service
trait ServiceContext extends ContextUtils with StrictLogging {
  val authContext: AuthContext = getAuthContext

  val useServiceId: Boolean = getUseServiceId
  val useProjectServiceId: Boolean = getUseProjectsServiceId

  val serviceId: Option[IAMStableServiceId] = getServiceId(useServiceId, authContext)
  val projectServiceId: Option[IAMStableServiceId] = getProjectsServiceId(useProjectServiceId, authContext)

  // this handles incoming calls as well as out-going auth calls that are made when accepting requests
  val endpointsActorSystem: ActorSystem = getEndpointsActorSystem
  // this handle outgoing calls
  val downstreamActorSystem: ActorSystem = getDownstreamActorSystem


  // see if the Platform is defined
  validatePlatformHost()

  val ignoreCerts: Boolean = {
    val ignore = platformICPToken.isDefined || Environment.isOnPrem // if ICP then allow all certs
    if (ignore)
      logger.info("Ignoring certificates when making HTTP connections")
    ignore
  }

  // the cached HTTP client loader for the authentication
  val authHttp: HttpClientBuilder = getHttpClientBuilder(
    ServiceNames.wmlRepositoryV4,
    AccessLogger.logAuthentication,
    endpointsActorSystem,
    allowAllCerts = ignoreCerts
  )

  // the cached HTTP client loader for the CAMS client
  val camsHttp: HttpClientBuilder = getHttpClientBuilder(
    ServiceNames.wmlRepositoryV4,
    AccessLogger.logDownstream,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )
  // the cached HTTP client loader for the ENV client
  val envHttp: HttpClientBuilder = getHttpClientBuilder(
    ServiceNames.wmlRepositoryV4,
    AccessLogger.logDownstream,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )

  // the method that decides what credentials to use with the platform services
  private def platformCredentialsProvider(identity: Identity): Future[HttpCredentialsProvider] = {
    platformCredsProvider(identity, isProject = false, useServiceId = platformUseServiceId)
  }

  // the method that decides what credentials to use with the container services
  private def containerSpaceCredentialsProvider(identity: Identity): Future[HttpCredentialsProvider] = {
    platformCredsProvider(identity, isProject = false, useServiceId = true)
  }

  private def containerProjectCredentialsProvider(identity: Identity): Future[HttpCredentialsProvider] = {
    platformCredsProvider(identity, isProject = true, useServiceId = true)
  }

  private def platformCredsProvider(identity: Identity, isProject: Boolean, useServiceId: Boolean): Future[HttpCredentialsProvider] = {
    implicit val ec: ExecutionContext = downstreamActorSystem.dispatcher

    if (useServiceId) {
      if (isProject) {
        if (useProjectServiceId) {
          return {
            for {
              token <- projectServiceId.get.createServiceIdToken()(downstreamActorSystem, authHttp)
            } yield {
              TokenProvider(token, extraHeaders = Vector(RawHeader(WMLUserIdHeader.name, identity.subject.id)))
            }
          }
        } else {
          logger.warn("Not able to use project service id for platform APIs as not configured")
        }
      } else {
        if (useServiceId) {
          return {
            for {
              token <- serviceId.get.createServiceIdToken()(downstreamActorSystem, authHttp)
            } yield {
              TokenProvider(token, extraHeaders = Vector(RawHeader(WMLUserIdHeader.name, identity.subject.id)))
            }
          }
        } else {
          logger.warn("Not able to use service id for platform APIs as not configured")
        }
      }
    }
    Future.successful {
      if (platformICPToken.isDefined) {
        identity.copy(rawToken = platformICPToken.get, subject = identity.subject.copy(subjectType = Subject.Service)).toCredentialsProvider()
      } else {
        identity.toCredentialsProvider()
      }
    }
  }

  val camsClient: CAMSClient = getCamsClient(useServiceId, downstreamActorSystem, camsHttp, platformCredentialsProvider)
  val environmentsClient: EnvironmentsClient = getEnvironmentsClient(useServiceId, downstreamActorSystem, envHttp, platformCredentialsProvider)
  val containerClient: ContainersClient = getContainersClient(useServiceId, downstreamActorSystem, envHttp, containerSpaceCredentialsProvider, containerProjectCredentialsProvider)

  val reportingAgent: ReportingAgent = ReportingAgent()

  // don't use these!!!

  // used to save the registration result
  var registrationResult: JsObject = JsObject(
    "cams-registration" -> JsString("none")
  )

  def getRegisterCAMSGlobalAssetsJob(system: ActorSystem,
                                     requestId: String): Future[List[AssetStatus]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val creator: Future[GlobalAssetTypesCreator] = for {
      serviceIdToken <- serviceId.get.createServiceIdToken()(system, camsHttp)
    } yield {
      reqId(Some(requestId))(() => logger.debug(s"Using service id token ${maskToken(serviceIdToken)} for asset registration"))
      val auth = OAuth2BearerToken(serviceIdToken)
      val camsUrl = Uri(getHost(camsClient.camsHost, camsClient.camsPort))
      GlobalAssetTypesCreator(
        camsUrl = camsUrl,
        // note that we do not provide a user_id as we don't have one and we don't need one
        authorization = Map(Authorization.name -> auth.value),
        listOnly = false,
        forceUpdate = false,
        clientLoader = camsHttp,
        retryPolicy = RetryFailure(3)
      )
    }
    for {
      creator <- creator
      status <- creator.declareGlobalAssetTypes(Some(requestId))(system)
    } yield {
      status
    }
  }

  def registerCAMSGlobalAssets(system: ActorSystem): Unit = {
    implicit val ec: ExecutionContext = system.dispatcher

    val requestId = "v4-repo-register-cams-assets-on-startup"
    val registrationTimeout = Duration.Inf // we do the timeout in the code

    val job = Try(getRegisterCAMSGlobalAssetsJob(system, requestId)) match {
      case Success(job) => job
      case Failure(exception) =>
        reqId(Some(requestId))(() => logger.error(s"Failed to set up the registration of the CAMS global asset types: ${exception.getMessage}", exception))
        return
    }

    // now we execute this
    system.dispatcher.execute(
      () => {
        Try {
          Await.result(job, registrationTimeout)
        } match {
          case Success(stats) =>
            Try {
              val json = stats.toJson
              reqId(Some(requestId))(() => logger.info(s"Asset registration finished: ${json.prettyPrint}"))
              // save the result
              registrationResult = JsObject(
                "cams_registration" -> json,
                "cams_registered_at" -> JsString(formatAsDate(System.currentTimeMillis()))
              )
            }
          case Failure(exception) =>
            exception match {
              case te: TimeoutException =>
                registrationResult = JsObject(
                  "cams_registration" -> JsString(s"Failed: ${te.getMessage}"),
                  "cams_registered_at" -> JsString(formatAsDate(System.currentTimeMillis()))
                )
                reqId(Some(requestId))(() => logger.error(s"Timeout whilst registering the CAMS global asset types: ${te.getMessage}", te))
              case e: Throwable =>
                registrationResult = JsObject(
                  "cams_registration" -> JsString(s"Failed: ${e.getMessage}"),
                  "cams_registered_at" -> JsString(formatAsDate(System.currentTimeMillis()))
                )
                reqId(Some(requestId))(() => logger.error(s"Failed to register the CAMS global asset types: ${e.getMessage}", e))
            }
        }
      }
    )
  }

  // this is the flag that controls quiesce (do not change this value)
  private val quiesceReadOnlyState: AtomicBoolean = new AtomicBoolean(false)

  def getQuiesceReadOnlyState: Boolean = quiesceReadOnlyState.get

  def startQuiesce(): Boolean = quiesceReadOnlyState.getAndSet(true)

  def stopQuiesce(): Boolean = quiesceReadOnlyState.getAndSet(false)
}
