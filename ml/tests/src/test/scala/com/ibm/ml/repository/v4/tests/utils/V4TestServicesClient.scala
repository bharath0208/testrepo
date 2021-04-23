/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.utils

import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.time.Duration

import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.{DeploymentJobDefinitionEntityRequest, DeploymentJobDefinitionResource, DeploymentJobDefinitionResources}
import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4ga.experiments.{ExperimentEntityRequest, ExperimentResource, ExperimentResources}
import com.ibm.analytics.wml.api.v4ga.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.functions.{FunctionEntityRequest, FunctionResource, FunctionResources}
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.model_definitions.{ModelDefinitionEntityRequest, ModelDefinitionResource, ModelDefinitionResources}
import com.ibm.analytics.wml.api.v4ga.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4ga.models.{ModelEntity, ModelResource, ModelResources}
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4ga.pipelines.{PipelineEntityRequest, PipelineResource, PipelineResources}
import com.ibm.analytics.wml.api.v4ga.remote_training_systems.RemoteTrainingSystemJsonFormat._
import com.ibm.analytics.wml.api.v4ga.remote_training_systems.{RemoteTrainingSystemEntityRequest, RemoteTrainingSystemResource, RemoteTrainingSystemResources}
import com.ibm.analytics.wml.api.v4ga.training_definitions.TrainingDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.training_definitions.{TrainingDefinitionEntityRequest, TrainingDefinitionResource, TrainingDefinitionResources}
import com.typesafe.config.Config
import javax.net.ssl.X509TrustManager
import org.apache.http._
import org.apache.http.client.config.RequestConfig
import org.apache.http.conn.ssl.NoopHostnameVerifier
import org.apache.http.impl.client.{CloseableHttpClient, HttpClientBuilder}
import org.apache.http.ssl.SSLContexts
import org.apache.logging.log4j.{LogManager, Logger}
import spray.json._

import scala.util.{Failure, Success, Try}

object V4TestServicesClient {
  // private val logger: Logger = LogManager.getLogger(classOf[V4TestServicesClient])

  def VERSION: String = "2020-04-25"

  val HEADER_REQUEST_ID = "Request-ID"
  val HEADER_GLOBAL_TRANSACTION_ID = "x-global-transaction-id"

  val CONFIG_SERVICE_URL: String = "service.host"
  val CONFIG_CLUSTER_URL: String = "cluster.url"
  val CONFIG_CLIENT_AUTH: String = "client.auth"
  val CONFIG_IAM_API_KEY: String = "fvt.iam.apikey"
  val ENV_WML_STABLE_API_KEY: String = "WML_STABLE_API_KEY"
  val ENV_WML_STABLE_API_KEY_PROJECTS: String = "WML_STABLE_API_KEY_PROJECTS"

  def getHttpsClient(noRetries: Boolean): CloseableHttpClient = {
    val timeout = 100000
    val maxConnections = 50
    val sslContext = SSLContexts.createDefault()
    sslContext.init(null, Array(new X509TrustManager {
      override def getAcceptedIssuers: Array[X509Certificate] = Array.empty[X509Certificate]

      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
    }), new SecureRandom())
    val requestConfig: RequestConfig = RequestConfig
      .custom()
      .setConnectTimeout(timeout)
      .setConnectionRequestTimeout(timeout)
      .setSocketTimeout(timeout)
      .build()
    val builder = HttpClientBuilder
      .create()
      // create a pooling client
      .setMaxConnTotal(maxConnections)
      .setMaxConnPerRoute(maxConnections)
      .setSSLHostnameVerifier(NoopHostnameVerifier.INSTANCE)
      .setSSLContext(sslContext)
      .setDefaultRequestConfig(requestConfig)
      .setConnectionManagerShared(true)
    if (noRetries) builder.disableAutomaticRetries()
    builder.build()
  }

  def getV4Client(config: Config,
                  noRetries: Boolean = false,
                  containerType: String = "project",
                  authTokenType: String = "user",
                  authHeaders: Option[Map[String, String]] = None): V4TestServicesClient = {
    val defaultUrl: String = "https://wml-fvt.ml.test.cloud.ibm.com"
    val serviceUrl: String = Try(config.getString(CONFIG_SERVICE_URL)).getOrElse(defaultUrl)
    val clusterUrl: String = Try(config.getString(CONFIG_CLUSTER_URL)).getOrElse(defaultUrl)
    val clientAuth: String = Try(config.getString(CONFIG_CLIENT_AUTH)).getOrElse("IAM")
    val iamApiKey: String = if (authTokenType == "service-id") {
      if (containerType == "space") {
        sys.env.getOrElse(ENV_WML_STABLE_API_KEY, throw new IllegalArgumentException(s"$ENV_WML_STABLE_API_KEY environment variable not set"))
      } else {
        sys.env.getOrElse(ENV_WML_STABLE_API_KEY_PROJECTS, throw new IllegalArgumentException(s"$ENV_WML_STABLE_API_KEY_PROJECTS environment variable not set"))
      }
    } else {
      Try(config.getString(CONFIG_IAM_API_KEY)).getOrElse(throw new IllegalArgumentException(s"$CONFIG_IAM_API_KEY environment variable not set"))
    }
    V4TestServicesClient(serviceUrl, clusterUrl, getHttpsClient(noRetries), config, VERSION, clientAuth, containerType, authTokenType, authHeaders, iamApiKey)
  }

  def getSize(len: Long): String = {
    if (len < 1024)
      s"${len}B"
    else if (len < (1024 * 1024))
      s"${len / 1024}KB"
    else
      s"${len / (1024 * 1024)}MB"
  }

  def getDuration(msecs: Long): String = {
    // this must not throw an exception
    Try(Duration.ofMillis(msecs).toString) match {
      case Success(value) => value
      case Failure(_) => s"$msecs ms"
    }
  }
}

case class V4TestServicesClient(serviceUrl: String,
                                clusterUrl: String,
                                httpClient: CloseableHttpClient,
                                config: Config,
                                version: String,
                                clientAuth: String,
                                containerType: String,
                                authTokenType: String,
                                authHeaders: Option[Map[String, String]] = None,
                                apiKey: String) extends AbstractTestServicesClient(
  serviceUrl = serviceUrl,
  clusterUrl = clusterUrl,
  httpClient = httpClient,
  config = config,
  version = version,
  clientAuth = clientAuth,
  containerType = containerType,
  authHeaders = authHeaders,
  apiKey = apiKey,
  authTokenType = authTokenType
) {
  private val logger: Logger = LogManager.getLogger(classOf[V4TestServicesClient])

  val MODELS: String = "/ml/v4/models"
  val MODELS_CONTENT_URI: String = "/content"
  val MODELS_DOWNLOAD_URI: String = "/download"
  val PIPELINES: String = "/ml/v4/pipelines"
  val EXPERIMENTS: String = "/ml/v4/experiments"
  val FUNCTIONS: String = "/ml/v4/functions"
  val FUNCTIONS_CONTENT_URI: String = "/code"
  val MODEL_DEFINITIONS: String = "/ml/v4/model_definitions"
  val MODEL_DEF_CONTENT_URI: String = "/model"
  val TRAINING_DEFINITIONS: String = "/ml/v4/training_definitions"
  val REMOTE_TRAINING_SYSTEMS: String = "/ml/v4/remote_training_systems"

  val DEPLOYMENT_JOB_DEFINITIONS: String = "/ml/v4/deployment_job_definitions"

  override def iamTokenGenerator: IAMTokenGenerator = {
    if (containerType.equalsIgnoreCase("space")) {
      GetIamToken(httpClient)
    } else {
      GetIamUAATokens(httpClient)
    }
  }

  // Methods below here are for the V4 API

  object models extends AbstractResourceWithContent[ModelEntity, ModelResource, ModelResources](
    MODELS,
    MODELS_CONTENT_URI,
    js => js.convertTo[ModelResource],
    js => js.convertTo[ModelResources],
    entity => entity.toJson,
    classOf[ModelResource],
    classOf[ModelResources]
  ) {

    def downloadAttachment(id: String,
                           attachmentId: String,
                           spaceId: Option[String] = None,
                           projectId: Option[String] = None,
                           requestId: Option[String] = None,
                           expectedStatus: Option[Int] = Some(200)): (String, Array[Header], Int) = {
      val uri = getUri(None, Some(id))
      getMethod(s"$uri$MODELS_CONTENT_URI/$attachmentId",
        queryParams = query(
          spaceId = spaceId,
          projectId = projectId
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        authHeaders = getAuthHeaders
      )
    }

    def downloadSingleAttachment(id: String,
                                 rev: Option[String] = None,
                                 name: Option[String] = None,
                                 contentFormat: Option[String] = None,
                                 spaceId: Option[String] = None,
                                 projectId: Option[String] = None,
                                 requestId: Option[String] = None,
                                 expectedStatus: Option[Int] = Some(200)): (String, Array[Header], Int) = {
      val uri = getUri(None, Some(id))
      getMethod(s"$uri$MODELS_DOWNLOAD_URI",
        queryParams = query(
          rev = rev,
          name = name,
          contentFormat = contentFormat,
          spaceId = spaceId,
          projectId = projectId
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        authHeaders = getAuthHeaders
      )
    }

    def deleteAttachment(id: String,
                         attachmentId: String,
                         spaceId: Option[String] = None,
                         projectId: Option[String] = None,
                         requestId: Option[String] = None,
                         expectedStatus: Option[Int] = Some(204)): (Option[String], Array[Header], Int) = {
      val uri = getUri(None, None)
      deleteMethod(s"$uri/$id$MODELS_CONTENT_URI/$attachmentId",
        queryParams = query(
          spaceId = spaceId,
          projectId = projectId
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        authHeaders = getAuthHeaders
      )
    }
  }

  object pipelines extends AbstractResource[PipelineEntityRequest, PipelineResource, PipelineResources](
    PIPELINES,
    js => js.convertTo[PipelineResource],
    js => js.convertTo[PipelineResources],
    entity => entity.toJson,
    classOf[PipelineResource],
    classOf[PipelineResources]
  )

  object experiments extends AbstractResource[ExperimentEntityRequest, ExperimentResource, ExperimentResources](
    EXPERIMENTS,
    js => js.convertTo[ExperimentResource],
    js => js.convertTo[ExperimentResources],
    entity => entity.toJson,
    classOf[ExperimentResource],
    classOf[ExperimentResources]
  )

  object functions extends AbstractResourceWithContent[FunctionEntityRequest, FunctionResource, FunctionResources](
    FUNCTIONS,
    FUNCTIONS_CONTENT_URI,
    js => js.convertTo[FunctionResource],
    js => js.convertTo[FunctionResources],
    entity => entity.toJson,
    classOf[FunctionResource],
    classOf[FunctionResources]
  )

  object modelDefinitions extends AbstractResourceWithContent[ModelDefinitionEntityRequest, ModelDefinitionResource, ModelDefinitionResources](
    MODEL_DEFINITIONS,
    MODEL_DEF_CONTENT_URI,
    js => js.convertTo[ModelDefinitionResource],
    js => js.convertTo[ModelDefinitionResources],
    entity => entity.toJson,
    classOf[ModelDefinitionResource],
    classOf[ModelDefinitionResources]
  )

  object trainingDefinitions extends AbstractResource[TrainingDefinitionEntityRequest, TrainingDefinitionResource, TrainingDefinitionResources](
    TRAINING_DEFINITIONS,
    js => js.convertTo[TrainingDefinitionResource],
    js => js.convertTo[TrainingDefinitionResources],
    entity => entity.toJson,
    classOf[TrainingDefinitionResource],
    classOf[TrainingDefinitionResources]
  )

  object deploymentJobDefinitions extends AbstractResource[DeploymentJobDefinitionEntityRequest, DeploymentJobDefinitionResource, DeploymentJobDefinitionResources](
    DEPLOYMENT_JOB_DEFINITIONS,
    js => js.convertTo[DeploymentJobDefinitionResource],
    js => js.convertTo[DeploymentJobDefinitionResources],
    entity => entity.toJson,
    classOf[DeploymentJobDefinitionResource],
    classOf[DeploymentJobDefinitionResources]
  )

  object remoteTrainingSystems extends AbstractResource[RemoteTrainingSystemEntityRequest, RemoteTrainingSystemResource, RemoteTrainingSystemResources](
    REMOTE_TRAINING_SYSTEMS,
    js => js.convertTo[RemoteTrainingSystemResource],
    js => js.convertTo[RemoteTrainingSystemResources],
    entity => entity.toJson,
    classOf[RemoteTrainingSystemResource],
    classOf[RemoteTrainingSystemResources]
  )
}
