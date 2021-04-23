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

import java.security.SecureRandom
import java.security.cert.X509Certificate
import java.time.Duration

import com.ibm.analytics.wml.api.v4.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4.experiments.{ExperimentResource, ExperimentResources}
import com.ibm.analytics.wml.api.v4.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4.functions.{FunctionResource, FunctionResources}
import com.ibm.analytics.wml.api.v4.libraries.LibraryJsonFormat._
import com.ibm.analytics.wml.api.v4.libraries.LibraryResource
import com.ibm.analytics.wml.api.v4.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4.models.{ModelResource, ModelResources}
import com.ibm.analytics.wml.api.v4.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4.pipelines.{PipelineResource, PipelineResources}
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.{DeploymentJobDefinitionResource, DeploymentJobDefinitionResources}
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.model_definitions.{ModelDefinitionResource, ModelDefinitionResources}
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.ml.repository.v4.tests.utils._
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

object V4BetaTestServicesClient {
  // private val logger: Logger = LogManager.getLogger(classOf[V4TestServicesClient])
  val HEADER_REQUEST_ID = "Request-ID"
  val HEADER_INSTANCE_ID = "ML-Instance-ID"
  val HEADER_INTERNAL_V4_SWITCH = "X-WML-INTERNAL-SWITCH-TO-NEW-V4"
  val HEADER_GLOBAL_TRANSACTION_ID = "x-global-transaction-id"
  val CONFIG_SERVICE_URL: String = "migration.host" // not service.host so that we have different hosts (use private endpoints) across tests
  val CONFIG_CLUSTER_URL: String = "cluster.url"
  val CONFIG_CLIENT_AUTH: String = "client.auth"
  val CONFIG_IAM_API_KEY_BETA: String = "fvt.iam.apikey_beta"

  def getV4Client(config: Config,
                  noRetries: Boolean = false,
                  containerType: String = "space",
                  authTokenType: String = "user",
                  authHeaders: Option[Map[String, String]] = None,
                  instanceId: String): V4BetaTestServicesClient = {
    val defaultUrl: String = "https://wml-fvt.ml.test.cloud.ibm.com"
    val serviceUrl: String = Try(config.getString(CONFIG_SERVICE_URL)).getOrElse(defaultUrl)
    val clusterUrl: String = Try(config.getString(CONFIG_CLUSTER_URL)).getOrElse(defaultUrl)
    val clientAuth: String = Try(config.getString(CONFIG_CLIENT_AUTH)).getOrElse("IAM")
    val iamApiKey: String = Try(config.getString(CONFIG_IAM_API_KEY_BETA)).getOrElse(throw new IllegalArgumentException(s"$CONFIG_IAM_API_KEY_BETA environment variable not set"))
    V4BetaTestServicesClient(serviceUrl, clusterUrl, getHttpsClient(noRetries), config, version = VERSION, clientAuth, containerType, authTokenType, authHeaders, instanceId, iamApiKey)
  }

  def VERSION: String = "2020-04-25"

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

case class V4BetaTestServicesClient(serviceUrl: String,
                                    clusterUrl: String,
                                    httpClient: CloseableHttpClient,
                                    config: Config,
                                    version: String,
                                    clientAuth: String,
                                    containerType: String,
                                    authTokenType: String,
                                    authHeaders: Option[Map[String, String]] = None,
                                    instanceId: String,
                                    apiKey: String) extends AbstractTestServicesClient(
  serviceUrl = serviceUrl,
  clusterUrl = clusterUrl,
  httpClient = httpClient,
  config = config,
  version = version,
  clientAuth = clientAuth,
  containerType = containerType,
  authTokenType = authTokenType,
  authHeaders = authHeaders,
  apiKey = apiKey
) {
  val MODELS: String = "/v4/models"
  val MODELS_CONTENT_URI: String = "/content"
  val LIBRARIES: String = "/v4/libraries"
  val LIBRARIES_CONTENT_URI: String = "/content"
  val MODELS_DOWNLOAD_URI: String = "/download"
  val PIPELINES: String = "/v4/pipelines"
  val EXPERIMENTS: String = "/v4/experiments"
  val FUNCTIONS: String = "/v4/functions"
  val FUNCTIONS_CONTENT_URI: String = "/content"
  val MODEL_DEFINITIONS: String = "/v4/model_definitions"
  val MODEL_DEF_CONTENT_URI: String = "/model"
  val TRAINING_DEFINITIONS: String = "/v4/training_definitions"
  val REMOTE_TRAINING_SYSTEMS: String = "/v4/remote_training_systems"
  val DEPLOYMENT_JOB_DEFINITIONS: String = "/v4/deployment_job_definitions"

  private val logger: Logger = LogManager.getLogger(classOf[V4BetaTestServicesClient])

  override def getRequestIdHeaders(requestId: Option[String]): Map[String, String] = {
    requestId match {
      case Some(reqId) =>
        Map(V4BetaTestServicesClient.HEADER_REQUEST_ID -> reqId,
          V4BetaTestServicesClient.HEADER_INSTANCE_ID -> instanceId,
          V4BetaTestServicesClient.HEADER_INTERNAL_V4_SWITCH -> "true")
      case None =>
        Map(V4BetaTestServicesClient.HEADER_INSTANCE_ID -> instanceId,
          V4BetaTestServicesClient.HEADER_INTERNAL_V4_SWITCH -> "true")
    }
  }

  // Methods below here are for the V4 API

  object models extends AbstractResourceWithContent[JsValue, ModelResource, ModelResources](
    MODELS,
    MODELS_CONTENT_URI,
    js => js.convertTo[ModelResource],
    js => js.convertTo[ModelResources],
    js => js,
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

  object pipelines extends AbstractResource[JsValue, PipelineResource, PipelineResources](
    PIPELINES,
    js => js.convertTo[PipelineResource],
    js => js.convertTo[PipelineResources],
    js => js,
    classOf[PipelineResource],
    classOf[PipelineResources],
    false
  )

  object experiments extends AbstractResource[JsValue, ExperimentResource, ExperimentResources](
    EXPERIMENTS,
    js => js.convertTo[ExperimentResource],
    js => js.convertTo[ExperimentResources],
    js => js,
    classOf[ExperimentResource],
    classOf[ExperimentResources]
  )

  object functions extends AbstractResourceWithContent[JsValue, FunctionResource, FunctionResources](
    FUNCTIONS,
    FUNCTIONS_CONTENT_URI,
    js => js.convertTo[FunctionResource],
    js => js.convertTo[FunctionResources],
    js => js,
    classOf[FunctionResource],
    classOf[FunctionResources]
  )

  object library extends AbstractResourceWithContent[JsValue, LibraryResource, LibraryResource](
    LIBRARIES,
    LIBRARIES_CONTENT_URI,
    js => js.convertTo[LibraryResource],
    js => js.convertTo[LibraryResource],
    js => js,
    classOf[LibraryResource],
    classOf[LibraryResource]
  )

  object deploymentJobDefinition extends AbstractResource[JsValue, DeploymentJobDefinitionResource, DeploymentJobDefinitionResources](
    DEPLOYMENT_JOB_DEFINITIONS,
    js => js.convertTo[DeploymentJobDefinitionResource],
    js => js.convertTo[DeploymentJobDefinitionResources],
    js => js,
    classOf[DeploymentJobDefinitionResource],
    classOf[DeploymentJobDefinitionResources]
  )

  object modelDefinition extends AbstractResourceWithContent[JsValue, ModelDefinitionResource, ModelDefinitionResources](
    MODEL_DEFINITIONS,
    MODEL_DEF_CONTENT_URI,
    js => js.convertTo[ModelDefinitionResource],
    js => js.convertTo[ModelDefinitionResources],
    js => js,
    classOf[ModelDefinitionResource],
    classOf[ModelDefinitionResources]
  )
}
