/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.api.v4

import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.{Directives, RequestContext, Route}
import com.ibm.analytics.wml.service.utils.http.{AuthorizationAction, AuthorizationRejection, ServiceAuth}
import com.ibm.analytics.wml.service.utils.security.AuthContext
import com.ibm.analytics.wml.service.utils.security.model.Subject._
import com.ibm.analytics.wml.service.utils.security.model.{Identity, ServiceInstance}
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.containers.Container
import com.ibm.analytics.wml.utils.security.{IAMConfig, IAMTenantAuthorizer, SecurityContext, Tenant}
import com.ibm.ml.repository.v4.migration.endpoints.MigrationEndpoints
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.service.MigrationServiceContext
import com.ibm.ml.repository.v4.utils.logging._
import com.ibm.ml.repository.v4.utils.{InvalidRequestEntityMessage, MissingJsonDocumentMessage, ServiceException, _}
import com.typesafe.scalalogging.StrictLogging
import org.slf4j.MDC
import spray.json._

import scala.collection.immutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MigrationV4API(msc: MigrationServiceContext) extends APIUtils with Directives with SprayJsonSupport with StrictLogging {
  implicit val system: ActorSystem = msc.endpointsActorSystem
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val builder: HttpClientBuilder = msc.authHttp
  implicit val ac: AuthContext = msc.authContext

  private def authenticate(req: RequestContext)
                          (f: Identity => Route): Route = {
    // we want to set a request id here if one is missing
    val requestId: String = getRequestId(req.request) match {
      case Some(requestId) =>
        requestId
      case None =>
        s"v4repo-migration-${UUID.randomUUID().toString}"
    }

    // set this for the authentication logs
    MDC.put(HEADER_REQUEST_ID, requestId)
    ServiceAuth.authenticate(req.request) { identity =>
      MDC.remove(HEADER_REQUEST_ID)
      // call the method
      f(identity.copy(requestId = Some(requestId)))
    }
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
    // set this for the authentication logs
    val pushed = if (identity.requestId.isDefined) {
      MDC.put(HEADER_REQUEST_ID, identity.requestId.get)
      true
    } else
      false

    // we set this flag so that the authorization will allow this call even if running on a v1 plan
    val request = ctx.request.withHeaders(Seq(RawHeader("x-wml-legacy-plan-access", "allow:v1")))
    ServiceAuth.authorizeV4Route(request, identity, container, role, actionName) { instance =>
      if (pushed)
        MDC.remove(HEADER_REQUEST_ID)
      f(instance)
    }
  }

  private def getSubjects(identity: Identity): Vector[com.ibm.analytics.wml.utils.security.Subject] = {
    Vector(
      com.ibm.analytics.wml.utils.security.Subject(
        subjectType = identity.subject.subjectType match {
          case User => Tenant.User
          case Service => Tenant.Service
          case DelegatedUser => Tenant.DelegatedUser
        },
        id = identity.subject.id,
        role = None,
        name = None // be safe and don't leak the users name identity.subject.name
      )
    ) ++ (
      if ((identity.subject.subjectType == DelegatedUser) && identity.subject.serviceId.isDefined)
        Vector(
          com.ibm.analytics.wml.utils.security.Subject(
            subjectType = Tenant.Service,
            id = identity.subject.serviceId.get,
            role = None,
            name = None
          )
        )
      else
        Vector()
      )
  }

  private def getTenant(identity: Identity, instanceId: String): Tenant = {
    Tenant(
      tenantId = identity.subject.id,
      rawToken = identity.rawToken,
      exp = 0L,
      tenantName = None,
      realm = if (identity.realm == com.ibm.analytics.wml.service.utils.security.model.Identity.IAM) Tenant.IAM else Tenant.ICP,
      planId = None,
      planName = None,
      instanceIdOpt = Some(instanceId),
      account = None,
      instanceOwner = None,
      scope = None,
      subject = Some(getSubjects(identity)),
      serviceEndpoint = None
    )
  }

  private val securityContext: Try[SecurityContext] = Try {
    Try(msc.authContext) match {
      case Success(ac) =>
        if (ac.secret.isEmpty)
          throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage("Failed to create security context"))
        else
          SecurityContext(ac.secret.get, IAMConfig())
      case Failure(exception) =>
        throw exception
    }
  }

  protected def authorizeV3(actionName: String,
                            instanceId: Option[String],
                            apiKey: Option[String],
                            role: AuthorizationAction)
                           (f: Option[ServiceInstance] => Route)
                           (implicit ctx: RequestContext,
                            identity: Identity,
                            system: ActorSystem,
                            builder: HttpClientBuilder): Route = {
    if (instanceId.isDefined) {

      // when we get here we assume that this is an identity from a user token
      if (ServiceAuth.isAllowedServiceId(identity)) {
        // Subject.toString handles printing of masked fields
        reqId(identity.requestId)(() => logger.warn(s"Handling call to migration API with WML service id ${identity.subject}"))
      }

      if (apiKey.isDefined) {
        // if apikey is defined, we will skip the check
        logger.debug("Using apikey for the old instance")
        f(None)
      } else {
        val action = s"${role.name.substring(0, 1).toUpperCase}${role.name.substring(1)}"

        def getIAMAction(action: AuthorizationAction): String = {
          action match {
            case AuthorizationAction.Viewer => "Viewer"
            case AuthorizationAction.Editor => msc.iamContext.pdp.editorAction
            case AuthorizationAction.Admin => msc.iamContext.pdp.administratorAction
          }
        }

        val check = for {
          sc <- Future.fromTry(securityContext)
          // may need to call IAMAuthorizer.authorize(tenant, instanceId.orElse(tenant.instanceIdOpt), ctx.iamConfig, action, ctx.cache, ctx.actionCache)
          _ <- IAMTenantAuthorizer.authorize(getTenant(identity, instanceId.get), getIAMAction(role), instanceId)(sc, system, builder) recoverWith {
            case t: Throwable =>
              logger.debug(s"IAMTenantAuthorizer rejected the $action access for ${identity.subject.id} to instance ${instanceId.get}")
              Future.failed(t)
          }
        } yield {
          logger.debug(s"IAMTenantAuthorizer authorized the $action access for ${identity.subject.id} to instance ${instanceId.get}")
          f(None)
        }

        onComplete(check) {
          case Success(value) => value
          case Failure(exception) =>
            reject(
              AuthorizationRejection(
                exception,
                identity,
                actionName,
                None
              )
            )
        }
      }

    } else {
      reject(
        AuthorizationRejection(
          new Exception("No instance id found"),
          identity,
          actionName,
          None
        )
      )
    }
  }

  protected def getAsObject(jsonEntity: JsValue, name: String): Option[JsObject] = {
    val obj: Option[JsValue] = Try(jsonEntity.asJsObject.fields.get(name)).getOrElse(None)
    obj match {
      case Some(js) =>
        js match {
          case obj: JsObject => Some(obj)
          case _ => None
        }
      case _ => None
    }
  }

  protected def getAsString(jsonEntity: JsValue, name: String): Option[String] = {
    val field: Option[JsValue] = Try(jsonEntity.asJsObject.fields.get(name)).getOrElse(None)
    field match {
      case Some(JsString(value)) if (value != null) && !value.trim.isEmpty => Some(value)
      case _ => None
    }
  }

  protected def getContainerFromRequestEntity(jsonEntity: JsValue): Container = {
    getContainer(getAsString(jsonEntity, "space_id"), getAsString(jsonEntity, "project_id"), spaceOnly = false, isPost = true)
  }

  protected def getInstanceIdFromRequestEntity(jsonEntity: JsValue): Option[String] = getValueFromOldInstance(jsonEntity, "instance_id")

  protected def getAPIKeyFromRequestEntity(jsonEntity: JsValue): Option[String] = getValueFromOldInstance(jsonEntity, "api_key")

  protected def getValueFromOldInstance(jsonEntity: JsValue, key: String): Option[String] = {
    getAsObject(jsonEntity, "old_instance") match {
      case Some(obj) =>
        getAsString(obj, key)
      case None =>
        None
    }
  }

  // As version is mandatory we check it here
  private def standardRouteHandling(actionName: String,
                                    requiresSpaceOrProjectId: Boolean = true)
                                   (f: (Map[String, String], Option[String], Option[String]) => Route)
                                   (implicit ctx: RequestContext,
                                    identity: Identity): Route = {
    try {
      enter(this.logger, ctx, identity)
      val now: Long = System.currentTimeMillis()
      //withRequestTimeout(getRequestApiTimeout(identity, ctx.request.method, ctx.request.uri.path)) {
      withRequestTimeoutResponse(_ => timeoutResponse(ctx.request, identity, System.currentTimeMillis() - now)) {
        parameters(
          QUERY_SPACE_ID.?,
          QUERY_PROJECT_ID.?
        ) { (spaceId: Option[String], projectId: Option[String]) =>
          if (requiresSpaceOrProjectId) {
            checkSpaceOrProjectId(spaceId, projectId)
          }
          parameterMap {
            params => {
              reqId(identity.requestId)(() => logger.info(s"[${identity.subject.id}] requested $actionName query: ${params.view}"))
              f(params, spaceId, projectId)
            }
          }
        }
      }
      //}
    }
    finally {
      exit(this.logger, ctx, identity)
    }
  }

  private def getQueryForLocation(spaceId: Option[String],
                                  projectId: Option[String]): (String, String) = {
    if (spaceId.isDefined) {
      ("space_id", spaceId.get)
    } else if (projectId.isDefined) {
      ("project_id", projectId.get)
    } else {
      val msg = "Either space_id or project_id has to be provided."
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }

  private def handleSuccess(status: StatusCode,
                            json: Option[JsValue],
                            assetId: Option[String],
                            params: Map[String, String],
                            actionName: String,
                            headers: immutable.Seq[HttpHeader] = Nil)
                           (implicit ctx: RequestContext,
                            identity: Identity): Route = {
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

  private def handleFailure(exception: Throwable,
                            actionName: String)
                           (implicit ctx: RequestContext,
                            identity: Identity): Route = {
    failWith(exception)
  }

  val endpoints: Route = getAllMigrationJobs ~
    createMigrationJob ~
    getMigrationJob ~
    deleteMigrationJob

  // public endpoints

  def getAllMigrationJobs: Route = {
    implicit ctx => {
      path("ml" / "v4" / "repository") {
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = "list-migration-jobs"
            standardRouteHandling(actionName) { (params, spaceId, projectId) =>
              authorize(actionName, getContainer(spaceId, projectId), AuthorizationAction.Viewer) { _ =>
                onComplete {
                  MigrationEndpoints.getAllMigrationJobs(identity, msc, spaceId, projectId)
                } {
                  case Success(mrs) =>
                    handleSuccess(
                      StatusCodes.OK,
                      Some(mrs.toJson),
                      None,
                      params,
                      actionName
                    )
                  case Failure(exception) =>
                    handleFailure(
                      exception,
                      actionName
                    )
                }
              }
            }
          }
        }
      }.apply(ctx)
    }
  }

  def createMigrationJob: Route = {
    implicit ctx => {
      path("ml" / "v4" / "repository") {
        post {
          authenticate(ctx) { implicit identity =>
            val actionName: String = "create-migration-job"
            standardRouteHandling(actionName, requiresSpaceOrProjectId = false) { (params, _, _) =>
              entity(as[JsValue]) {
                jsonEntity => {
                  if ((jsonEntity == null) || (jsonEntity == JsNull))
                    throw ServiceException(StatusCodes.BadRequest, MissingJsonDocumentMessage())
                  authorize(actionName, getContainerFromRequestEntity(jsonEntity), AuthorizationAction.Editor) { _ =>
                    authorizeV3(actionName, getInstanceIdFromRequestEntity(jsonEntity), getAPIKeyFromRequestEntity(jsonEntity), AuthorizationAction.Editor) { _ =>
                      onComplete {
                        MigrationEndpoints.createMigrationJob(identity, msc, jsonEntity)
                      } {
                        case Success(mr) =>
                          handleSuccess(
                            StatusCodes.Accepted,
                            Some(mr.toJson),
                            None,
                            params,
                            actionName,
                            // return the url with the migration id
                            headers = List({
                              val uri = ctx.request.uri
                              headers.Location(uri.withPath(Uri.Path(s"${uri.path}/${mr.migrationId}"))
                                .withQuery(Uri.Query(getQueryForLocation(
                                  getAsString(jsonEntity, "space_id"),
                                  getAsString(jsonEntity, "project_id")
                                ))))
                            })
                          )
                        case Failure(exception) =>
                          handleFailure(
                            exception,
                            actionName
                          )
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }.apply(ctx)
    }
  }

  def getMigrationJob: Route = {
    implicit ctx => {
      path("ml" / "v4" / "repository" / Segment) { id =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = "get-migration-job"
            standardRouteHandling(actionName) { (params, spaceId, projectId) =>
              authorize(actionName, getContainer(spaceId, projectId), AuthorizationAction.Viewer) { _ =>
                onComplete {
                  MigrationEndpoints.getMigrationJob(identity, msc, id, spaceId, projectId)
                } {
                  case Success(mr) =>
                    handleSuccess(
                      StatusCodes.OK,
                      Some(mr.toJson),
                      None,
                      params,
                      actionName
                    )
                  case Failure(exception) =>
                    handleFailure(
                      exception,
                      actionName
                    )
                }
              }
            }
          }
        }
      }.apply(ctx)
    }
  }

  def deleteMigrationJob: Route = {
    implicit ctx => {
      path("ml" / "v4" / "repository" / Segment) { id =>
        delete {
          authenticate(ctx) { implicit identity =>
            val actionName: String = "delete-migration-job"
            standardRouteHandling(actionName) { (params, spaceId, projectId) =>
              parameters(
                "hard_delete".as[Boolean] ? false
              ) { (hardDelete: Boolean) => {
                authorize(actionName, getContainer(spaceId, projectId), AuthorizationAction.Viewer) { _ =>
                  onComplete {
                    MigrationEndpoints.deleteMigrationJob(identity, msc, id, hardDelete, spaceId, projectId)
                  } {
                    case Success(_) =>
                      handleSuccess(
                        StatusCodes.NoContent,
                        None,
                        None,
                        params,
                        actionName
                      )
                    case Failure(exception) =>
                      handleFailure(
                        exception,
                        actionName
                      )
                  }
                }
              }
              }
            }
          }
        }
      }.apply(ctx)
    }
  }
}
