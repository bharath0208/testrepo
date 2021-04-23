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

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.containers.ContainersClient
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableProjectsServiceId, IAMStableServiceId}
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.service.utils.security.{AuthContext, StableServiceId}
import com.ibm.analytics.wml.utils.clients.http._
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import spray.json.{JsNumber, JsObject, JsString, JsValue}

import java.io.File
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

trait ContextUtils extends StrictLogging {
  val platformUseServiceId: Boolean = Try(config.getBoolean("service.platform.use-service-id")).getOrElse(false)
  val platformICPToken: Option[String] = Try(Some(config.getString("service.platform.icp.token"))).getOrElse(None)

  def getAsFile(config: Config,
                path: String): File = {
    new File(config.getString(path)).getAbsoluteFile
  }

  protected def loadFile(file: File): String = {
    Using.resource(Source.fromFile(file)) {
      source =>
        source.getLines().mkString("\n")
    }
  }

  protected def loadFile(config: Config,
                         path: String): String = {
    loadFile(getAsFile(config, path))
  }

  lazy val iamContext: IAMContext = IAMContext()

  private lazy val ac: Try[AuthContext] = {
    logger.info("Initializing AuthContext...")

    def iamContextToString(iamConfig: Option[IAMContext]): Option[JsValue] = {
      iamConfig match {
        case Some(config) =>
          Some(
            JsObject(
              "host" -> JsString(config.host),
              "port" -> JsNumber(config.port),
              "keysEndpoint" -> JsString(config.keysEndpoint),
              "tokenEndpoint" -> JsString(config.tokenEndpoint)
            )
          )
        case None =>
          None
      }
    }

    def authContextToString(authContext: AuthContext): String = {
      val iam: Option[JsValue] = iamContextToString(authContext.iamConfig)
      val fields: Map[String, JsValue] = if (iam.isDefined) Map("iamConfig" -> iam.get) else Map()
      logPrint(JsObject(fields))
    }

    val path = getWmlAuth match {
      case AUTH_ICP => Some("service.validateTokenService.icp.pubkey.path")
      case AUTH_IAM => Some("service.validateTokenService.ml.pubkey.path")
      case AUTH_NONE => Some("service.validateTokenService.ml.pubkey.path").filter(config.hasPath)
    }
    Try {
      val secret = path match {
        case Some(path) =>
          Some(loadFile(config, path))
        case None =>
          None
      }
      AuthContext(
        secret = secret,
        iamConfig = Some(iamContext)
      )
    } match {
      case Success(ac) =>
        logger.info(s"Initialized AuthContext: ${authContextToString(ac)}")
        Success(ac)
      case Failure(exception) =>
        logger.error(s"Failed to load public key for authentication context from $path: ${exception.getMessage}", exception)
        Failure(exception)
    }
  }

  protected def getAuthContext: AuthContext = {
    ac match {
      case Success(ac) =>
        ac
      case Failure(exception) =>
        throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage("Failed to load authentication context"), Some(exception))
    }
  }

  protected def getUseServiceId: Boolean = {
    getWmlAuth match {
      case AUTH_IAM =>
        if (StableServiceId.verifyEnvironment.isSuccess) {
          logger.info(s"Service id enabled for this environment $environment")
          true
        } else {
          logger.warn(s"Service id is not configured for this environment $environment")
          false
        }
      case _ =>
        logger.warn(s"Service id is not enabled for this environment $environment")
        false
    }
  }

  protected def getUseProjectsServiceId: Boolean = {
    getWmlAuth match {
      case AUTH_IAM =>
        if (Try(IAMStableProjectsServiceId.getProjectsServiceIdApiKey).isSuccess) {
          logger.info(s"Projects service id enabled for this environment $environment")
          true
        } else {
          logger.warn(s"Projects service id is not configured for this environment $environment")
          false
        }
      case _ =>
        logger.warn(s"Projects service id is not enabled for this environment $environment")
        false
    }
  }

  protected def getServiceId(useServiceId: Boolean,
                             authContext: AuthContext): Option[IAMStableServiceId] = {
    if (useServiceId) Some(IAMStableServiceId()(authContext.iamConfig.get)) else None
  }

  protected def getProjectsServiceId(useServiceId: Boolean,
                                     authContext: AuthContext): Option[IAMStableProjectsServiceId] = {
    if (useServiceId) Some(IAMStableProjectsServiceId()(authContext.iamConfig.get)) else None
  }

  protected def getEndpointsActorSystem: ActorSystem = ContextDefinitions.createActorSystem(config = config, systemName = ContextDefinitions.SYSTEM_ENDPOINTS)

  protected def getDownstreamActorSystem: ActorSystem = ContextDefinitions.createActorSystem(config = config, systemName = ContextDefinitions.SYSTEM_DOWNSTREAM_SERVICES)

  protected def getHttpClientBuilder(serviceName: String,
                                     reporting: AkkaHttpClient.Reporting,
                                     system: ActorSystem,
                                     allowAllCerts: Boolean = false): HttpClientBuilder = CachedHttpClientBuilder(
    service = Some(serviceName),
    reporting = reporting,
    allowAllCerts = allowAllCerts
  )(system)

  protected def validatePlatformHost(): Unit = {
    if (platformHost.isFailure) {
      val msg = s"Platform host name is not defined in the configuration"
      logger.error(msg)
      throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage(msg), Some(platformHost.failed.get))
    } else if (platformPort.isFailure) {
      val msg = s"Platform port is not defined in the configuration"
      logger.error(msg)
      throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage(msg), Some(platformPort.failed.get))
    } else {
      logger.info(s"Using platform ${getHost(platformHost.get, platformPort.get)}")
    }
  }

  // do this brute force approach for now
  // only retry on certain return codes
  protected lazy val retryPolicy: RetryPolicy = {
    val codes: HashSet[Int] = Try {
      config.getIntList("service.ml-repository.v4-services.retry-codes")
    } match {
      case Success(values) =>
        val vs: mutable.Set[Int] = mutable.Set()
        val it = values.iterator()
        while (it.hasNext) {
          vs.add(it.next())
        }
        HashSet() ++ vs.toSet
      case Failure(exception) =>
        logger.warn(s"Failed to get retry codes: ${ServiceException.getExceptionMessage(exception)}")
        HashSet() ++ (401 to 527).toSet
    }
    val numRetries: Int = Try(config.getInt("service.ml-repository.v4-services.number-retries")).getOrElse(3)
    val delayTimeMillis: Long = Try(config.getDuration("service.ml-repository.v4-services.delay-time").toMillis).getOrElse(100)

    logger.info(s"Retry for downstream V4 services: number of retries is $numRetries, delay between retries is $delayTimeMillis msecs and retrying for status codes ${codes.mkString(",")}")

    EnhancedRetryOnCodesWithDelay(
      numRetries = numRetries,
      delayTimeMillis = delayTimeMillis.toInt,
      retryCodes = codes
    )
  }

  protected def getCamsClient(useServiceId: Boolean,
                              system: ActorSystem,
                              builder: HttpClientBuilder,
                              credentialsProvider: Identity => Future[HttpCredentialsProvider]): CAMSClient = {
    val client = new CAMSClient(platformHost.get, platformPort.get)(builder, credentialsProvider, system, retryPolicy)
    logger.info(s"Created CAMS client using ${getHost(client.camsHost, client.camsPort)}${getAuthDescription(useServiceId)}")
    client
  }

  protected def getEnvironmentsClient(useServiceId: Boolean,
                                      system: ActorSystem,
                                      builder: HttpClientBuilder,
                                      credentialsProvider: Identity => Future[HttpCredentialsProvider]): EnvironmentsClient = {
    val client = new EnvironmentsClient(platformHost.get, platformPort.get)(builder, credentialsProvider, system, retryPolicy)
    logger.info(s"Created Environments client using ${getHost(client.envsHost, client.envsPort)}${getAuthDescription(useServiceId)}")
    client
  }

  protected def getContainersClient(useServiceId: Boolean,
                                    system: ActorSystem,
                                    builder: HttpClientBuilder,
                                    spaceCredentialsProvider: Identity => Future[HttpCredentialsProvider],
                                    projectCredentialsProvider: Identity => Future[HttpCredentialsProvider]): ContainersClient = {
    val client = new ContainersClient(platformHost.get, platformPort.get)(builder, projectCredentialsProvider, spaceCredentialsProvider, system, retryPolicy)
    logger.info(s"Created Containers client using ${getHost(client.platformHost, client.platformPort)}${getAuthDescription(useServiceId)}")
    client
  }

  private def getAuthDescription(useServiceId: Boolean): String = {
    if (platformUseServiceId && useServiceId)
      " with WML service id"
    else if (platformICPToken.isDefined)
      s"""" with ICP token "${platformICPToken.get}""""
    else
      ""
  }

}
