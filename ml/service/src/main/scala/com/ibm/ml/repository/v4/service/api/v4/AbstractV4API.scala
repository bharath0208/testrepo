/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.api.v4

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.service.utils.http.{AuthorizationAction, AuthorizationRejection, ServiceAuth}
import com.ibm.analytics.wml.service.utils.security.AuthContext
import com.ibm.analytics.wml.service.utils.security.model.Subject.Service
import com.ibm.analytics.wml.service.utils.security.model._
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.containers.Container
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.cache.StorageTypeCache
import com.ibm.ml.repository.v4.service.reporting.MLRepositoryState
import com.ibm.ml.repository.v4.service.reporting.ReportingAgent.ActionType
import com.ibm.ml.repository.v4.utils.errors.ServiceExceptionHandler
import com.ibm.ml.repository.v4.utils.logging._
import com.ibm.ml.repository.v4.utils.{APIUtils, InvalidQueryParameterMessage, ServiceException, UnexpectedQueryParameters, _}
import com.typesafe.scalalogging.{Logger, StrictLogging}
import org.slf4j.{LoggerFactory, MDC}
import spray.json.{JsObject, JsString, JsValue}

import java.util.UUID
import scala.collection.{immutable, mutable}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

class AbstractV4API(sc: ServiceContext) extends APIUtils with Directives with StrictLogging {
  private val securityLogger: Logger = Logger(LoggerFactory.getLogger("security"))

  protected def checkVersion(version: String): VersionDate = {
    VersionDate(version) match {
      case Success(vd) =>
        vd
      case Failure(exception) if exception.isInstanceOf[InvalidDateException] =>
        throw ServiceException(StatusCodes.BadRequest, InvalidQueryParameterMessage(parameter = QUERY_VERSION, value = version, reason = Some(ServiceException.getExceptionMessage(exception))))
      case Failure(_) =>
        throw ServiceException(StatusCodes.BadRequest, InvalidQueryParameterMessage(parameter = QUERY_VERSION, value = version))
    }
  }

  private def getRemainingParameters(parameters: Map[String, String],
                                     expected: Seq[String],
                                     startsWith: Seq[String]): Map[String, String] = {
    val remaining: mutable.Map[String, String] = mutable.Map()
    for (param <- parameters) {
      if (param._1.nonEmpty)
        if (expected.contains(param._1)) {
          // ok expected
        } else {
          var found = false
          for (sw <- startsWith) {
            if (param._1.startsWith(sw)) {
              found = true
            }
          }
          if (!found) remaining += param
        }
    }
    remaining.toMap
  }

  protected def checkParameters(parameters: Map[String, String],
                                expected: Seq[String],
                                startsWith: Seq[String] = Seq(),
                                strict: Boolean = false)
                               (implicit ctx: RequestContext,
                                identity: Identity): Vector[String] = {
    val rem: Map[String, String] = getRemainingParameters(parameters, expected, startsWith)
    if (rem.nonEmpty) {
      reqId(() => logger.debug(s"Found unexpected query parameters '${rem.mkString(",")}' for request ${ctx.request.method} ${ctx.request.uri}"))
      if (strict) {
        throw ServiceException(StatusCodes.BadRequest, UnexpectedQueryParameters(rem.keySet.toVector, expected))
      }
    }
    // leave it to the caller to do something if unexpected parameters found and not strict mode
    rem.keySet.toVector
  }

  protected def authenticate(req: RequestContext)(f: Identity => Route): Route = {
    implicit val system: ActorSystem = sc.endpointsActorSystem
    implicit val ac: AuthContext = sc.authContext
    implicit val httpClientLoader: HttpClientBuilder = sc.authHttp

    // we want to set a request id here if one is missing
    val requestId: String = getRequestId(req.request) match {
      case Some(requestId) =>
        requestId
      case None =>
        s"v4repo-${UUID.randomUUID().toString}"
    }

    // set this for the authentication logs
    MDC.put(HEADER_REQUEST_ID, requestId)
    ServiceAuth.authenticate(req.request) { identity =>
      MDC.remove(HEADER_REQUEST_ID)
      // call the method
      f(identity)
    }
  }

  protected def sendResultSuccess(identity: Identity,
                                  container: Container,
                                  instance: Option[ServiceInstance],
                                  request: HttpRequest,
                                  state: MLRepositoryState,
                                  httpResponseCode: Int,
                                  patch: Option[JsValue] = None): Future[Unit] = {
    sc.reportingAgent.reportEvent(identity, container, instance, request, state, httpResponseCode, patch)(sc.downstreamActorSystem, sc.authHttp)
  }

  protected def sendResultFailure(identity: Identity,
                                  container: Container,
                                  instance: Option[ServiceInstance],
                                  request: HttpRequest,
                                  state: MLRepositoryState,
                                  status: Int): Future[Unit] = {
    sc.reportingAgent.reportEvent(identity, container, instance, request, state, status, None)(sc.downstreamActorSystem, sc.authHttp)
  }

  protected def getStandardHandlingParameters(requiresSpaceOrProjectId: Boolean,
                                              spaceOnly: Boolean): Seq[String] = {
    if (spaceOnly)
      Seq(QUERY_VERSION, QUERY_SPACE_ID)
    else if (requiresSpaceOrProjectId)
      Seq(QUERY_VERSION, QUERY_SPACE_ID, QUERY_PROJECT_ID)
    else
      Seq(QUERY_VERSION)
  }

  // As version is mandatory we check it here
  protected def standardRouteHandling(actionName: String,
                                      requiresSpaceOrProjectId: Boolean = true,
                                      spaceOnly: Boolean = false)
                                     (f: (Map[String, String], VersionDate, Option[String], Option[String]) => Route)
                                     (implicit ctx: RequestContext,
                                      identity: Identity): Route = {
    try {
      enter(this.logger, ctx, identity)
      val now: Long = System.currentTimeMillis()
      // withRequestTimeout(getRequestApiTimeout(identity, ctx.request.method, ctx.request.uri.path)) {
      withRequestTimeoutResponse(_ => timeoutResponse(ctx.request, identity, System.currentTimeMillis() - now)) {
        parameters(
          QUERY_VERSION,
          QUERY_SPACE_ID.?,
          QUERY_PROJECT_ID.?
        ) { (version: String, spaceId: Option[String], projectId: Option[String]) =>
          if (requiresSpaceOrProjectId) {
            checkSpaceOrProjectId(spaceId, projectId, spaceOnly)
          }
          parameterMap {
            params => {
              reqId(identity.requestId)(() => logger.info(s"[${identity.subject.id}] requested $actionName query: ${params.mkString(",")}"))
              val versionDate = checkVersion(version)
              f(params, versionDate, spaceId, projectId)
            }
          }
        }
      }
      // }
    }
    finally {
      exit(this.logger, ctx, identity)
    }
  }

  protected def callContext(versionDate: VersionDate, container: Container)
                           (f: CallContext => Route)
                           (implicit ctx: RequestContext,
                            sc: ServiceContext,
                            ec: ExecutionContext,
                            identity: Identity): Route = {

    onComplete {
      StorageTypeCache.get(container)
    } {
      case Success(containerStorageType) => f(CallContext(identity, versionDate, container, containerStorageType))
      case Failure(exception) => failWith(exception)
    }
  }

  private def requiresContainerAuthorization(identity: Identity): Boolean = {

    // for service user we do not check since we do not have the right user id
    if (identity.subject.subjectType == Service) false
    else {
      if (isPublicCloud) {
        // if authorization is being skipped then we do it only if we are a user token or a delegated user
        ServiceAuth.skipMemberAuthorization(identity)
      } else {
        // in ICP we always do at least member authorization
        true
      }
    }
  }

  private def authorizeContainerCheck(actionName: String,
                                      container: Container,
                                      role: AuthorizationAction,
                                      instance: Option[ServiceInstance])
                                     (f: Option[ServiceInstance] => Route)
                                     (implicit ctx: RequestContext,
                                      identity: Identity,
                                      system: ActorSystem,
                                      builder: HttpClientBuilder): Route = {
    val now: Long = System.currentTimeMillis()

    if (requiresContainerAuthorization(identity)) {
      implicit val ec: ExecutionContext = system.dispatcher

      val authorize: Future[Option[String]] = for {
        context <- ServiceAuth.authorizeMemberInContainer(container, identity, role)
      } yield {
        context.instanceId
      }

      onComplete(authorize) {
        case Success(_) =>
          securityLogger.info(s"Approved extra container authorization  ${ctx.request.method.value} ${ctx.request.uri} for ${identity.subject.id} in $container in ${formatAsDuration(System.currentTimeMillis() - now)}")
          f(instance)
        case Failure(exception) =>
          securityLogger.warn(s"Rejected extra container authorization ${ctx.request.method.value} ${ctx.request.uri} for ${identity.subject.id} in $container : ${exception.getMessage} in ${formatAsDuration(System.currentTimeMillis() - now)}")
          reject(
            AuthorizationRejection(
              exception,
              identity,
              actionName,
              None
            )
          )
      }
    } else
      f(instance)
  }

  protected def authorize(actionName: String,
                          container: Container,
                          role: AuthorizationAction)
                         (f: Option[ServiceInstance] => Route)
                         (implicit ctx: RequestContext,
                          identity: Identity,
                          system: ActorSystem,
                          builder: HttpClientBuilder): Route = {
    // for now this authorizes ALL calls - we could use
    // 'requiresContainerAuthorization(identity)' to authorize only service id calls

    val pushed = if (identity.requestId.isDefined) {
      MDC.put(HEADER_REQUEST_ID, identity.requestId.get)
      true
    } else
      false

    // if we are in ICP and read-only mode and this is not a viewer action then reject
    if (isPrivateCloud) {
      if (sc.getQuiesceReadOnlyState && (role != AuthorizationAction.Viewer)) {
        reject(AuthorizationRejection(ServiceException(StatusCodes.ServiceUnavailable, QuiesceModeEnabled()), identity, actionName, None))
      } else {
        authorizeContainerCheck(actionName, container, role, None)(f)
      }
    } else {
      ServiceAuth.authorizeV4Route(ctx.request, identity, container, role, actionName) { instance =>
        if (pushed)
          MDC.remove(HEADER_REQUEST_ID)
        authorizeContainerCheck(actionName, container, role, instance)(f)
      }
    }
  }

  protected def handleResponse(response: HttpResponse,
                               container: Container,
                               assetId: Option[String],
                               assetName: Option[String],
                               params: Map[String, String],
                               actionType: ActionType,
                               actionName: String,
                               assetType: String,
                               instance: Option[ServiceInstance])
                              (implicit ctx: RequestContext,
                               identity: Identity): Future[Unit] = {
    if (response.status.isSuccess()) {
      sendResultSuccess(
        identity = identity,
        container = container,
        instance = instance,
        request = ctx.request,
        state = MLRepositoryState(
          assetId = assetId.getOrElse("none"),
          assetType = assetType,
          assetName = assetName,
          actionType = actionType,
          actionName = actionName,
          state = Some(JsObject(params.map(entry => entry._1 -> JsString(entry._2))))
        ),
        httpResponseCode = response.status.intValue,
        patch = None
      )
    } else {
      sendResultFailure(
        identity = identity,
        container = container,
        instance = instance,
        request = ctx.request,
        state = MLRepositoryState(
          assetId = assetId.getOrElse("none"),
          assetType = assetType,
          assetName = assetName,
          actionType = actionType,
          actionName = actionName,
          state = None
        ),
        status = response.status.intValue()
      )
    }
  }

  protected def handleSuccess(container: Container,
                              status: StatusCode,
                              json: Option[JsValue],
                              assetId: Option[String],
                              assetName: Option[String],
                              params: Map[String, String],
                              actionType: ActionType,
                              actionName: String,
                              assetType: String,
                              instance: Option[ServiceInstance],
                              headers: immutable.Seq[HttpHeader] = Nil)
                             (implicit ctx: RequestContext,
                              identity: Identity): Route = {
    onComplete {
      sendResultSuccess(
        identity = identity,
        container = container,
        instance = instance,
        request = ctx.request,
        state = MLRepositoryState(
          assetId = assetId.getOrElse("none"),
          assetType = assetType,
          assetName = assetName,
          actionType = actionType,
          actionName = actionName,
          state = Some(JsObject(params.map(entry => entry._1 -> JsString(entry._2))))
        ),
        httpResponseCode = status.intValue,
        patch = None
      )
    } { _ =>
      complete(
        json match {
          case Some(js) =>
            HttpResponse(
              status = status,
              entity = getJsonEntity(js),
              headers = headers
            )
          case None =>
            HttpResponse(
              status = status,
              headers = headers
            )
        }
      )
    }
  }

  protected def handleFailure(container: Container,
                              exception: Throwable,
                              assetId: Option[String],
                              assetName: Option[String],
                              actionType: ActionType,
                              actionName: String,
                              assetType: String,
                              instance: Option[ServiceInstance])
                             (implicit ctx: RequestContext,
                              identity: Identity): Route = {
    onComplete {
      sendResultFailure(
        identity = identity,
        container = container,
        instance = instance,
        request = ctx.request,
        state = MLRepositoryState(
          assetId = assetId.getOrElse("none"),
          assetType = assetType,
          assetName = assetName,
          actionType = actionType,
          actionName = actionName,
          state = None
        ),
        status = ServiceExceptionHandler.getStatusCode(exception)
      )
    } { _ =>
      failWith(exception)
    }
  }

  protected def getAsString(jsonEntity: JsValue, name: String): Option[String] = {
    jsonEntity.asJsObject.fields.get(name) match {
      case Some(JsString(value)) if (value != null) && value.trim.nonEmpty => Some(value)
      case _ => None
    }
  }

  protected def getContainerFromRequestEntity(jsonEntity: JsValue,
                                              spaceOnly: Boolean): Container = {
    getContainer(getAsString(jsonEntity, "space_id"), getAsString(jsonEntity, "project_id"), spaceOnly, isPost = true)
  }
}
