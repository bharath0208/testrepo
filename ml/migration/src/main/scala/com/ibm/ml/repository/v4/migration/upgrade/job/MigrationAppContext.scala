/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.job

import java.util.Base64

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.model.headers.RawHeader
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.repository.RepositoryClient
import com.ibm.analytics.wml.service.utils.authorization.AuthorizationClient
import com.ibm.analytics.wml.service.utils.http.WMLUserIdHeader
import com.ibm.analytics.wml.service.utils.security.iam.IAMStableServiceId
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.ibm.analytics.wml.service.utils.security.{AuthContext, BasicCredentials, Environment, TokenCredentials}
import com.ibm.analytics.wml.utils.ServiceNames
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.clients.http.{HttpClientBuilder, TokenProvider}
import com.ibm.ml.repository.v4.migration.upgrade.cloudant.{MigrationDBMethods, SimpleCloudantClient}
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.migration.utils.v4beta.repository.V4BetaRepositoryClient
import com.ibm.ml.repository.v4.utils.logging.{AccessLogger, ExceptionLogger}
import com.ibm.ml.repository.v4.utils.{AUTH_IAM, AUTH_ICP, ContextUtils, FailedToGetServiceId, InvalidConfigurationMessage, ServiceException, config, getWmlAuth, wmlHost, wmlPort}
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

trait MigrationAppContext extends ContextUtils with MigrationConstant with StrictLogging {

  val authContext: AuthContext = getAuthContext

  val useServiceId: Boolean = getUseServiceId

  val serviceId: Option[IAMStableServiceId] = getServiceId(useServiceId, authContext)

  // this handle outgoing calls
  val downstreamActorSystem: ActorSystem = getDownstreamActorSystem

  // see if the Platform is defined
  validatePlatformHost()

  val ignoreCerts: Boolean = platformICPToken.isDefined || Environment.isOnPrem // if ICP then allow all certs

  // the cached HTTP client loader for the CAMS client
  val camsHttp: HttpClientBuilder = getHttpClientBuilder(ServiceNames.wmlRepositoryMigrationV4,
    AccessLogger.logDownstream,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )
  // the cached HTTP client loader for the ENV client
  val envHttp: HttpClientBuilder = getHttpClientBuilder(ServiceNames.wmlRepositoryMigrationV4,
    AccessLogger.logDownstream,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )

  // the cached HTTP client loader for the old repository client
  val v4RepoHttp: HttpClientBuilder = getHttpClientBuilder(ServiceNames.wmlRepositoryMigrationV4,
    AccessLogger.logDownstream,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )

  // the cached HTTP client loader for the new repository client
  val mlRepoHttp: HttpClientBuilder = getHttpClientBuilder(ServiceNames.wmlRepositoryMigrationV4,
    AccessLogger.logDownstream,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )

  // the cached HTTP client loader for the authentication
  val authHttp: HttpClientBuilder = getHttpClientBuilder(ServiceNames.wmlRepositoryMigrationV4,
    AccessLogger.logAuthentication,
    downstreamActorSystem,
    allowAllCerts = ignoreCerts
  )

  private def getServiceIdToken: Future[String] = {
    implicit val ec: ExecutionContext = downstreamActorSystem.dispatcher
    if (useServiceId) {
      for {
        token <- serviceId.get.createServiceIdToken()(downstreamActorSystem, authHttp)
      } yield {
        token
      }
    } else {
      val se = ServiceException(StatusCodes.InternalServerError, FailedToGetServiceId())
      ExceptionLogger.log(None, se, None, callStack = true)
      throw se
    }
  }

  private def getICPServiceIdCredentials: String = {
    sys.env.get("service-id-credentials") match {
      case Some(credentials) => credentials
      case _ =>
        val se = ServiceException(StatusCodes.InternalServerError, FailedToGetServiceId())
        ExceptionLogger.log(None, se, None, callStack = true)
        throw se
    }
  }

  private def serviceIdCredentialsProvider(identity: Identity)
                                          (implicit system: ActorSystem): Future[HttpCredentialsProvider] = {
    implicit val ec: ExecutionContext = system.dispatcher

    getWmlAuth match {
      case AUTH_IAM =>
        for {
          token <- getServiceIdToken
        } yield {
          TokenProvider(token, extraHeaders = Vector(RawHeader(WMLUserIdHeader.name, identity.subject.id)))
        }
      case AUTH_ICP =>
        Future.successful(identity.copy(rawToken = getICPServiceIdCredentials, subject = identity.subject.copy(subjectType = Subject.Service)).toCredentialsProvider())
      case _ =>
        Future.failed(ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage("Failed identity job environment")))
    }
  }

  def getIdentity: Future[Identity] = {
    implicit val ec: ExecutionContext = downstreamActorSystem.dispatcher
    getWmlAuth match {
      case AUTH_IAM =>
        for {
          token <- getServiceIdToken
          identity <- TokenCredentials(token)(authContext, authHttp, downstreamActorSystem).validate()
        } yield {
          identity
        }
      case AUTH_ICP =>
        val basicCredentials = getICPServiceIdCredentials
        val decoded = new String(Base64.getDecoder.decode(basicCredentials)).trim
        val up = decoded.split(":")
        BasicCredentials(up(0), up(1)).validate()
      case _ =>
        Future.failed(ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage("Failed identity job environment")))
    }
  }

  val environmentsClient: EnvironmentsClient = {
    implicit val system: ActorSystem = downstreamActorSystem
    getEnvironmentsClient(useServiceId, system, envHttp, serviceIdCredentialsProvider)
  }
  val v4BetaRepositoryClient: V4BetaRepositoryClient = {
    V4BetaRepositoryClient.createV4BetaRepositoryClient(
      wmlHost.get,
      wmlPort,
      getICPServiceIdCredentials
    )(
      v4RepoHttp,
      downstreamActorSystem,
      iamContext
    )
  }

  val camsClient: CAMSClient = {
    implicit val system: ActorSystem = downstreamActorSystem
    getCamsClient(useServiceId, downstreamActorSystem, camsHttp, serviceIdCredentialsProvider)
  }

  val mlRepositoryClient: RepositoryClient = {
    implicit val system: ActorSystem = downstreamActorSystem
    new RepositoryClient(wmlHost.get, wmlPort)(mlRepoHttp, serviceIdCredentialsProvider, system, retryPolicy)
  }

  val (dbUrl, dbUsername, dbPassword, dbName) = SimpleCloudantClient.getDBConfig(config)

  val cloudantClient: SimpleCloudantClient = SimpleCloudantClient(dbUrl, dbName, dbUsername, dbPassword, Some(CLOUDANT_UPGRADE_DESIGN_DOC_PATH), ignoreCerts)(downstreamActorSystem)
  val dbMethods: MigrationDBMethods = MigrationDBMethods(cloudantClient)
  val authClient: AuthorizationClient = AuthorizationClient()(authHttp, downstreamActorSystem)
}
