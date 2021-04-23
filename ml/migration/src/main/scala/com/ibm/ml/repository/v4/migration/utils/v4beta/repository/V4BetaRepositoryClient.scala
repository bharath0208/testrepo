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

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.Authorization
import akka.http.scaladsl.unmarshalling.Unmarshal
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.ibm.analytics.wml.api.v4.experiments._
import com.ibm.analytics.wml.api.v4.functions._
import com.ibm.analytics.wml.api.v4.libraries.{LibraryResource, LibraryResources}
import com.ibm.analytics.wml.api.v4.models._
import com.ibm.analytics.wml.api.v4.pipelines._
import com.ibm.analytics.wml.api.v4.runtimes.{RuntimeResource, RuntimeResources}
import com.ibm.analytics.wml.service.utils.http.WMLUserIdHeader
import com.ibm.analytics.wml.service.utils.security.iam.IAMContext
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.clients.http.{AkkaHttpClient, HttpClientBuilder, HttpError}
import com.ibm.analytics.wml.utils.errors.MLFailures.format
import com.ibm.analytics.wml.utils.errors.{MLFailure, MLFailures}
import com.ibm.ml.repository.v4.migration.models.{OldInstance, V4BetaResources}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * A client to access the V4Beta repository service.
 */
object V4BetaRepositoryClient extends StrictLogging {
  val HEADER_GLOBAL_TRANSACTION_ID: String = "x-global-transaction-id"

  /**
   * Create a V4Beta repository client using the default credentials provider.
   *
   * @param host              The host.
   * @param port              The port.
   * @param httpClientBuilder The builder for the AkkaHttpClient.
   * @param system            The actor system to use for the calls.
   * @return The V4Beta repository client.
   */
  def apply(host: String,
            port: Int,
            httpClientBuilder: HttpClientBuilder,
            system: ActorSystem): V4BetaRepositoryClient = {
    new V4BetaRepositoryClient(host, port)(httpClientBuilder, getCredentialsProvider, system)
  }

  /**
   * The default credentials provider.
   *
   * This method uses the rawToken (which must include the scheme)
   * as well as the requestId if it exists in the identity.
   *
   * @param identity The identity for the user.
   * @return The credential headers that are required by the V4Beta repository.
   */
  def getCredentialsProvider(identity: Identity): Future[Map[String, String]] = Future.successful {
    // the default behaviour is just to pass down the token in the identity
    getTokenHeaders(identity) ++ getRequestIdHeaders(identity)
  }

  /**
   * @param identity The identity for the user.
   * @return The authorization headers that are required by the V4Beta repository.
   */
  def getTokenHeaders(identity: Identity): Map[String, String] = {
    logger.debug(s"Found ${Authorization.name} '${identity.authScheme}' header")
    Map(Authorization.name -> identity.getAuthentication())
  }

  /**
   * This will return the HTTP header for the requestId if it exists in the identity.
   *
   * @param identity The identity for the user.
   * @return The HTTP header or an empty map.
   */
  def getRequestIdHeaders(identity: Identity): Map[String, String] = {
    if (identity.requestId.isDefined) {
      Map(HEADER_GLOBAL_TRANSACTION_ID -> identity.requestId.get)
    } else
      Map()
  }

  val MODELS: String = "/v4/models"
  val CONTENT_URI: String = "/content"
  val PIPELINES: String = "/v4/pipelines"
  val EXPERIMENTS: String = "/v4/experiments"
  val FUNCTIONS: String = "/v4/functions"
  val RUNTIMES: String = "/v4/runtimes"
  val LIBRARIES: String = "/v4/libraries"

  /**
   * This method should be called every time a V4 beta client is required.
   *
   * The method will handle all caching aspects.
   *
   * @param oldInstance       The old instance details that contains the credentials to the user's instance.
   * @param httpClientBuilder The HTTP client builder.
   * @param system            The actor system for the client.
   * @return The V4 beta client.
   */
  def createV4BetaRepositoryClient(host: String,
                                   port: Int,
                                   oldInstance: OldInstance)
                                  (implicit httpClientBuilder: HttpClientBuilder,
                                   system: ActorSystem,
                                   iamContext: IAMContext): V4BetaRepositoryClient = {
    def credentialsProvider(identity: Identity): Future[Map[String, String]] = {
      CachedCredentialsProvider.getCredentials(identity, oldInstance)
    }

    new V4BetaRepositoryClient(host, port)(httpClientBuilder, credentialsProvider, system)
  }

  def createV4BetaRepositoryClient(host: String,
                                   port: Int,
                                   BasicServiceId: String)
                                  (implicit httpClientBuilder: HttpClientBuilder,
                                   system: ActorSystem,
                                   iamContext: IAMContext): V4BetaRepositoryClient = {
    def credentialsProvider(identity: Identity): Future[Map[String, String]] = {
      Future.successful(Map(
        Authorization.name -> s"Basic $BasicServiceId",
        WMLUserIdHeader.name -> identity.subject.id
      ))
    }

    new V4BetaRepositoryClient(host, port)(httpClientBuilder, credentialsProvider, system)
  }

}

import com.ibm.ml.repository.v4.migration.utils.v4beta.repository.V4BetaRepositoryClient._

/**
 * Create a V4 repository client using the given credentials provider.
 *
 * @param host                The host.
 * @param port                The port.
 * @param httpClientBuilder   The builder for the AkkaHttpClient.
 * @param credentialsProvider The credentials provider.
 * @param system              The actor system to use for the calls.
 * @return The V4 repository client.
 */
case class V4BetaRepositoryClient(host: String,
                                  port: Int)
                                 (implicit httpClientBuilder: HttpClientBuilder,
                                  credentialsProvider: Identity => Future[Map[String, String]],
                                  system: ActorSystem) extends DefaultJsonProtocol with StrictLogging {
  private implicit val ec: ExecutionContext = system.dispatcher

  private implicit val credentials: Option[HttpCredentialsProvider] = None

  private def getCredentialsProvider(identity: Identity): Future[Map[String, String]] = {
    credentialsProvider(identity)
  }

  protected def getHttpClient: AkkaHttpClient = {
    httpClientBuilder.get(host, port, HttpClientBuilder.Keys.REPOSITORY_CLIENT)
  }

  private def checkHttpStatus(identity: Identity,
                              resp: HttpResponse): Future[HttpResponse] = {
    if (resp.status.isSuccess) {
      logger.debug(s"V4 repository operation successfully executed. Status code: ${resp.status.value}")
      Future.successful(resp)
    } else {
      {
        for {
          body <- Unmarshal(resp.entity).to[String]
        } yield {
          logger.error(s"V4 repository operation failed with error response: $body")
          throw JsonParser(body).convertTo[MLFailures]
        }
      } recoverWith {
        case e: MLFailures =>
          logger.debug(s"V4BetaRepository call failed: ${e.getMessage()}", e)
          Try(resp.discardEntityBytes())
          Future.failed(e)
        case t: Throwable => logger.error(s"Error during the error response json deserialization: ${t.getMessage}", t)
          Try(resp.discardEntityBytes())
          Future.failed(
            getFailure(
              identity = identity,
              code = "unknown_downstream_repository_failure",
              message = s"Failed to process the V4 repository operation. Status code: ${resp.status.intValue}"
            )
          )
      }
    }
  }

  private def convertHttpResponse[T](identity: Identity,
                                     resp: HttpResponse,
                                     convert: JsValue => T): Future[T] = {
    {
      for {
        content <- Unmarshal(resp.entity).to[String]
      } yield {
        logger.debug(s"response: $content")
        convert(JsonParser(content))
      }
    } recoverWith {
      case t: Throwable =>
        logger.error(s"Error during the response json deserialization: ${t.getMessage}. Status code: ${resp.status.intValue}", t)
        Try {
          resp.discardEntityBytes()
        }
        Future.failed(
          getFailure(
            identity = identity,
            code = "repository_deserialization_failure",
            message = s"Failed to deserialize the V4 repository response. Status code: ${resp.status.intValue}"
          )
        )
    }
  }

  private def getFailure(identity: Identity,
                         code: String,
                         message: String): MLFailures = {
    val trace: String = identity.requestId.getOrElse("unknown")
    MLFailures(
      Seq(
        MLFailure(
          code = code,
          message = message
        )
      ),
      trace
    )
  }

  private def getActionName(uri: String, action: String): Option[String] = {
    val uriPattern = "^/ml/v4/(.+)$".r

    uriPattern.findFirstMatchIn(uri) match {
      case Some(prefix) => Some(s"${prefix}_$action")
      case None => None // should never get here
    }
  }

  private def getHeaders(identity: Identity,
                         extraHeaders: Option[Map[String, String]]): Future[Map[String, String]] = {
    for {
      credentials <- getCredentialsProvider(identity)
    } yield {
      credentials ++
        (if (extraHeaders.nonEmpty) extraHeaders.get else Map()) ++
        getRequestIdHeaders(identity)
    }
  }

  private def createUri(uri: String,
                        id: Option[String],
                        queries: (String, Option[String])*): String = {
    val queryBuilder = Query.newBuilder
    for (query <- queries) {
      if (query._2.isDefined)
        queryBuilder += ((query._1, query._2.get))
    }
    val result = Uri(if (id.nonEmpty) s"$uri/${id.get}" else uri).withQuery(queryBuilder.result())
    result.toString
  }

  private def httpExceptionHandler(result: Future[HttpResponse]): Future[HttpResponse] = {
    result.recoverWith {
      case he: HttpError =>
        logger.error("Repository call failed due to HTTP error", he)
        Future.failed(MLFailure("repository_connection_error", "The service is experiencing some downstream errors, please re-try the request"))
      case t: Throwable => Future.failed(t)
    }
  }

  protected abstract class AbstractResource[ResourceClass, ResourceClasses](uri: String,
                                                                            convertResource: JsValue => ResourceClass,
                                                                            convertResources: JsValue => ResourceClasses,
                                                                            resource: Class[_],
                                                                            resources: Class[_])
                                                                           (implicit resourcesFormatter: RootJsonFormat[ResourceClasses])
    extends V4BetaRepositoryService[ResourceClass, ResourceClasses]
      with RepositoryClientConstant {


    protected def makeHttpCall(identity: Identity,
                               extraHeaders: Option[Map[String, String]],
                               callActionFunction: Map[String, String] => Future[HttpResponse]): Future[HttpResponse] = {
      for {
        headers <- getHeaders(identity, extraHeaders)
        response1 <- callActionFunction(headers)
        response <- checkHttpStatus(identity, response1)
      } yield {
        response
      }
    }

    private def makeResourceClassHttpCall(identity: Identity,
                                          extraHeaders: Option[Map[String, String]],
                                          callActionFunction: Map[String, String] => Future[HttpResponse]): Future[ResourceClass] = {
      for {
        response <- makeHttpCall(identity, extraHeaders, callActionFunction)
        resource <- convertHttpResponse(identity, response, convertResource)
      } yield {
        resource
      }
    }

    private def makeResourceClassesHttpCall(identity: Identity,
                                            extraHeaders: Option[Map[String, String]],
                                            callActionFunction: Map[String, String] => Future[HttpResponse]): Future[ResourceClasses] = {
      for {
        response <- makeHttpCall(identity, extraHeaders, callActionFunction)
        resources <- convertHttpResponse(identity, response, convertResources)
      } yield {
        resources
      }
    }

    // just for tests
    private def makeJsonHttpCall(identity: Identity,
                                 extraHeaders: Option[Map[String, String]],
                                 callActionFunction: Map[String, String] => Future[HttpResponse]): Future[JsValue] = {
      def convertJsonResponse(response: HttpResponse): Future[JsValue] = {
        {
          for {
            content <- Unmarshal(response.entity).to[String]
          } yield {
            logger.debug(s"response: $content")
            JsonParser(content)
          }
        } recoverWith {
          case t: Throwable =>
            logger.error(s"Error during the response json deserialization: ${t.getMessage}. Status code: ${response.status.intValue}", t)
            Try {
              response.discardEntityBytes()
            }
            Future.failed(
              getFailure(
                identity = identity,
                code = "repository_deserialization_failure",
                message = s"Failed to deserialize the V4 repository response. Status code: ${response.status.intValue}"
              )
            )
        }
      }

      for {
        response <- makeHttpCall(identity, extraHeaders, callActionFunction)
        resource <- convertJsonResponse(response)
      } yield {
        resource
      }
    }

    override def list(identity: Identity,
                      spaceId: Option[String] = None,
                      projectId: Option[String] = None,
                      start: Option[String] = None,
                      limit: Option[Int] = None,
                      tagValue: Option[String] = None,
                      extraHeaders: Option[Map[String, String]] = None): Future[ResourceClasses] = {

      makeResourceClassesHttpCall(identity, extraHeaders, headers => {
        httpExceptionHandler(getHttpClient.get(
          uri = createUri(
            uri,
            None,
            (QUERY_SPACE_ID, spaceId),
            (QUERY_PROJECT_ID, projectId),
            (QUERY_START, start),
            (QUERY_LIMIT, if (limit.nonEmpty) Some(limit.get.toString) else None),
            (QUERY_TAG_VALUE, tagValue)
          ),
          action = getActionName(uri, "list"),
          headers = headers
        ))
      })
    }

    override def listAllIds(identity: Identity,
                            spaceId: Option[String] = None,
                            projectId: Option[String] = None): Future[Seq[String]] = {
      def getResult(start: Option[String], result: Seq[String]): Future[Seq[String]] = {
        list(identity, start = start, limit = Some(100), spaceId = spaceId, projectId = projectId).flatMap { resources =>
          import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
          val res = resources.toJson.convertTo[V4BetaResources]

          if (res.next.isDefined) {
            val nextUrl = res.next.get.href
            val query = Uri(nextUrl).query().toMap
            val newStart = query.get(QUERY_START)
            val newResult = res.resources.map(_.metadata.id)
            getResult(newStart, result ++ newResult)
          } else {
            Future.successful(result ++ res.resources.map(_.metadata.id))
          }
        }
      }

      getResult(None, Seq.empty)
    }

    override def get(identity: Identity,
                     id: String,
                     rev: Option[String] = None,
                     spaceId: Option[String] = None,
                     projectId: Option[String] = None,
                     extraHeaders: Option[Map[String, String]] = None): Future[ResourceClass] = {

      makeResourceClassHttpCall(identity, extraHeaders, headers => {
        httpExceptionHandler(getHttpClient.get(
          uri = createUri(
            uri,
            Some(id),
            (QUERY_SPACE_ID, spaceId),
            (QUERY_PROJECT_ID, projectId),
            (QUERY_REV, rev)
          ),
          action = getActionName(uri, "get"),
          headers = headers
        ))
      })
    }

    // just for tests
    override def createRaw(identity: Identity,
                           entity: JsValue,
                           spaceId: Option[String] = None,
                           projectId: Option[String] = None,
                           extraHeaders: Option[Map[String, String]] = None): Future[JsValue] = {
      makeJsonHttpCall(identity, extraHeaders, headers => {
        httpExceptionHandler(getHttpClient.postJson(
          uri = createUri(
            uri,
            None,
            (QUERY_SPACE_ID, spaceId),
            (QUERY_PROJECT_ID, projectId)
          ),
          action = getActionName(uri, "post-json"),
          headers = headers,
          body = entity
        ))
      })
    }
  }

  protected abstract class AbstractResourceWithSingleContent[ResourceClass, ResourceClasses](uri: String,
                                                                                             contentUri: String,
                                                                                             convertResource: JsValue => ResourceClass,
                                                                                             convertResources: JsValue => ResourceClasses,
                                                                                             resource: Class[_],
                                                                                             resources: Class[_])
                                                                                            (implicit resourcesFormatter: RootJsonFormat[ResourceClasses])
    extends AbstractResource[ResourceClass, ResourceClasses](
      uri = uri,
      convertResource = convertResource,
      convertResources = convertResources,
      resource = resource,
      resources = resources
    ) with V4BetaRepositorySingleContentService[ResourceClass, ResourceClasses]
      with RepositoryClientConstant {

    private def getContentUri(id: String): Option[String] = {
      if (contentUri.startsWith("/"))
        Some(s"$id$contentUri")
      else
        Some(s"$id/$contentUri")
    }

    private def contentName: String = {
      if (contentUri.startsWith("/"))
        contentUri.substring(1)
      else
        contentUri
    }

    override def download(identity: Identity,
                          id: String,
                          rev: Option[String] = None,
                          format: Option[String] = None,
                          artifact: Option[String] = None,
                          spaceId: Option[String] = None,
                          projectId: Option[String] = None,
                          extraHeaders: Option[Map[String, String]] = None): Future[Source[ByteString, Any]] = {
      makeHttpCall(identity, extraHeaders, headers => {
        httpExceptionHandler(getHttpClient.get(
          uri = createUri(
            uri,
            getContentUri(id),
            (QUERY_SPACE_ID, spaceId),
            (QUERY_PROJECT_ID, projectId),
            (QUERY_REV, rev),
            (QUERY_FORMAT, format),
            (QUERY_ARTIFACT, artifact)
          ),
          action = getActionName(uri, s"download_$contentName"),
          headers = headers
        ))
      }).map(_.entity.dataBytes)
    }
  }

  trait RepositoryClientConstant {
    val QUERY_SPACE_ID = "space_id"
    val QUERY_PROJECT_ID = "project_id"
    val QUERY_REV = "rev"
    val QUERY_NAME = "name"
    val QUERY_CONTENT_FORMAT = "content_format"
    val QUERY_TAG_VALUE = "tag.value"
    val QUERY_START = "start"
    val QUERY_LIMIT = "limit"
    val QUERY_NEXT = "next"
    val QUERY_PIPELINE_NODE_ID = "pipeline_node_id"
    val QUERY_FORMAT = "format"
    val QUERY_ARTIFACT = "artifact"
  }

  import com.ibm.analytics.wml.api.v4.experiments.ExperimentJsonFormat._

  object experiments extends AbstractResource[ExperimentResource, ExperimentResources](
    EXPERIMENTS,
    js => js.convertTo[ExperimentResource],
    js => js.convertTo[ExperimentResources],
    classOf[ExperimentResource],
    classOf[ExperimentResources]
  )

  import com.ibm.analytics.wml.api.v4.functions.FunctionJsonFormat._

  object functions extends AbstractResourceWithSingleContent[FunctionResource, FunctionResources](
    FUNCTIONS,
    CONTENT_URI,
    js => js.convertTo[FunctionResource],
    js => js.convertTo[FunctionResources],
    classOf[FunctionResource],
    classOf[FunctionResources]
  )

  import com.ibm.analytics.wml.api.v4.models.ModelJsonFormat._

  object models extends AbstractResourceWithSingleContent[ModelResource, ModelResources](
    MODELS,
    CONTENT_URI,
    js => js.convertTo[ModelResource],
    js => js.convertTo[ModelResources],
    classOf[ModelResource],
    classOf[ModelResources]
  )

  import com.ibm.analytics.wml.api.v4.pipelines.PipelineJsonFormat._

  object pipelines extends AbstractResource[PipelineResource, PipelineResources](
    PIPELINES,
    js => js.convertTo[PipelineResource],
    js => js.convertTo[PipelineResources],
    classOf[PipelineResource],
    classOf[PipelineResources]
  )

  import com.ibm.analytics.wml.api.v4.runtimes.RuntimeJsonFormat._

  object runtimes extends AbstractResource[RuntimeResource, RuntimeResources](
    RUNTIMES,
    js => js.convertTo[RuntimeResource],
    js => js.convertTo[RuntimeResources],
    classOf[RuntimeResource],
    classOf[RuntimeResources]
  )

  import com.ibm.analytics.wml.api.v4.libraries.LibraryJsonFormat._

  object library extends AbstractResourceWithSingleContent[LibraryResource, LibraryResources](
    LIBRARIES,
    CONTENT_URI,
    js => js.convertTo[LibraryResource],
    js => js.convertTo[LibraryResources],
    classOf[ModelResource],
    classOf[ModelResources]
  )

}
