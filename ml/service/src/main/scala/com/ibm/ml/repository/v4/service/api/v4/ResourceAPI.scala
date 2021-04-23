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
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model._
import akka.http.scaladsl.server._
import com.ibm.analytics.wml.service.utils.http.AuthorizationAction
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.containers.Container
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.reporting.ReportingAgent
import com.ibm.ml.repository.v4.service.resources.ResourceMethods
import com.ibm.ml.repository.v4.service.utils.AssetConstant
import com.ibm.ml.repository.v4.utils._
import spray.json._

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

// https://doc.akka.io/docs/akka-http/10.2/routing-dsl/style-guide.html
case class ResourceAPI(resourceMethods: ResourceMethods)
                      (implicit sc: ServiceContext) extends AbstractV4API(sc) with SprayJsonSupport with AssetConstant {
  implicit val system: ActorSystem = sc.downstreamActorSystem
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val builder: HttpClientBuilder = sc.authHttp

  private def tracedGetAllResources()(implicit ctx: RequestContext): Route = {
    authenticate(ctx) { implicit identity =>
      val actionName: String = s"list-${resourceMethods.name}s"
      standardRouteHandling(actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
        parameters(
          QUERY_START.?,
          QUERY_LIMIT.as[Int] ? QUERY_LIMIT_DEFAULT,
          QUERY_TAG_VALUE.?,
          QUERY_SEARCH.?
        ) { (start: Option[String], limit: Int, tagValue: Option[String], search: Option[String]) =>
          checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++ Seq(QUERY_START, QUERY_LIMIT, QUERY_TAG_VALUE, QUERY_SEARCH))(ctx, identity)
          val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
          authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
            callContext(versionDate, container) { implicit callContext =>
              onComplete {
                resourceMethods.resourceMethods.getAll(
                  ignoreBlank(identity, start, QUERY_START),
                  Some(limit),
                  ignoreBlank(identity, tagValue, QUERY_TAG_VALUE),
                  ignoreBlank(identity, search, QUERY_SEARCH)
                )
              } {
                case Success(ers) =>
                  handleSuccess(
                    container = container,
                    status = StatusCodes.OK,
                    json = Some(ers),
                    assetId = None,
                    instance = instance,
                    assetName = None,
                    params = params,
                    actionType = ReportingAgent.search,
                    actionName = actionName,
                    assetType = resourceMethods.name
                  )
                case Failure(exception) =>
                  handleFailure(
                    container = container,
                    exception = exception,
                    assetId = None,
                    instance = instance,
                    assetName = None,
                    actionType = ReportingAgent.search,
                    actionName = actionName,
                    assetType = resourceMethods.name
                  )
              }
            }
          }
        }
      }
    }
  }

  def getAllResources: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s") {
        get {
          tracedGetAllResources()
        }
      }
    }.apply(ctx)
  }

  def createResource: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s") {
        post {
          extractMatchedPath { matched =>
            authenticate(ctx) { implicit identity =>
              val actionName: String = s"create-${resourceMethods.name}"
              standardRouteHandling(actionName, requiresSpaceOrProjectId = false, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, _, _) =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = false, resourceMethods.spaceOnly))(ctx, identity)
                entity(as[JsValue]) {
                  jsonEntity => {
                    if ((jsonEntity == null) || (jsonEntity == JsNull))
                      throw ServiceException(StatusCodes.BadRequest, MissingJsonDocumentMessage())
                    val container: Container = getContainerFromRequestEntity(jsonEntity, spaceOnly = resourceMethods.spaceOnly)
                    authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                      callContext(versionDate, container) { implicit callContext =>
                        onComplete {
                          resourceMethods.resourceMethods.create(jsonEntity)
                        } {
                          case Success((er, sc)) =>
                            // when we have content location as input field it is async
                            val statusCode = jsonEntity.asJsObject.fields.get(CONTENT_LOCATION).
                              map(_ => StatusCodes.Accepted).getOrElse(sc)

                            val locationHeaders: Seq[HttpHeader] = if (statusCode == StatusCodes.Created) {
                              // there should be an easier way to do this?
                              Try(er.asJsObject.fields(METADATA).asJsObject.fields("id")) match {
                                case Success(JsString(id)) =>
                                  List(headers.Location(Uri(s"$matched/$id")))
                                case _ =>
                                  logger.warn(s"Failed to get resource id for 'Location' header from ${logPrint(er)}")
                                  Nil
                              }
                            } else
                              Nil

                            handleSuccess(
                              container = container,
                              status = statusCode,
                              json = Some(er),
                              assetId = None,
                              instance = instance,
                              assetName = None,
                              params = params,
                              actionType = ReportingAgent.create,
                              actionName = actionName,
                              assetType = resourceMethods.name,
                              headers = locationHeaders
                            )
                          case Failure(exception) =>
                            handleFailure(
                              container = container,
                              exception = exception,
                              assetId = None,
                              instance = instance,
                              assetName = None,
                              actionType = ReportingAgent.create,
                              actionName = actionName,
                              assetType = resourceMethods.name
                            )
                        }
                      }
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

  def patchResource: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment) { id =>
        patch {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"update-${resourceMethods.name}"
            standardRouteHandling(actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly))(ctx, identity)
              entity(as[JsValue]) {
                jsonPatch => {
                  if ((jsonPatch == null) || (jsonPatch == JsNull))
                    throw ServiceException(StatusCodes.BadRequest, MissingJsonDocumentMessage())
                  val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                  authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                    callContext(versionDate, container) { implicit callContext =>
                      onComplete {
                        resourceMethods.resourceMethods.update(id, jsonPatch)
                      } {
                        case Success(er) =>
                          handleSuccess(
                            container = container,
                            status = StatusCodes.OK,
                            json = Some(er),
                            assetId = None,
                            instance = instance,
                            assetName = None,
                            params = params,
                            actionType = ReportingAgent.update,
                            actionName = actionName,
                            assetType = resourceMethods.name
                          )
                        case Failure(exception) =>
                          handleFailure(
                            container = container,
                            exception = exception,
                            assetId = None,
                            instance = instance,
                            assetName = None,
                            actionType = ReportingAgent.update,
                            actionName = actionName,
                            assetType = resourceMethods.name
                          )
                      }
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

  def getResource: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment) { id =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"get-${resourceMethods.name}"
            standardRouteHandling(actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_REV.?
              ) { rev: Option[String] =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++ Seq(QUERY_REV))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    onComplete {
                      resourceMethods.resourceMethods.get(
                        id,
                        ignoreBlank(identity, rev, QUERY_REV)
                      )
                    } {
                      case Success(resource) =>
                        handleSuccess(
                          container = container,
                          status = StatusCodes.OK,
                          json = Some(resource),
                          assetId = Some(id),
                          instance = instance,
                          assetName = None,
                          params = params,
                          actionType = ReportingAgent.read,
                          actionName = actionName,
                          assetType = resourceMethods.name
                        )
                      case Failure(exception) =>
                        handleFailure(
                          container = container,
                          exception = exception,
                          assetId = None,
                          instance = instance,
                          assetName = None,
                          actionType = ReportingAgent.read,
                          actionName = actionName,
                          assetType = resourceMethods.name
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

  def deleteResource: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment) { id =>
        delete {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"delete-${resourceMethods.name}"
            standardRouteHandling(actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_PURGE_ON_DELETE.as[Boolean] ? QUERY_PURGE_ON_DELETE_DEFAULT
              ) { purgeOnDelete: Boolean =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++ Seq(QUERY_PURGE_ON_DELETE))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    onComplete {
                      resourceMethods.resourceMethods.delete(id, purgeOnDelete)
                    } {
                      case Success(_) =>
                        handleSuccess(
                          container = container,
                          status = StatusCodes.NoContent,
                          json = None,
                          assetId = Some(id),
                          instance = instance,
                          assetName = None,
                          params = params,
                          actionType = ReportingAgent.delete,
                          actionName = actionName,
                          assetType = resourceMethods.name
                        )
                      case Failure(exception) =>
                        handleFailure(
                          container = container,
                          exception = exception,
                          assetId = None,
                          instance = instance,
                          assetName = None,
                          actionType = ReportingAgent.delete,
                          actionName = actionName,
                          assetType = resourceMethods.name
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

  def getAllRevisions: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "revisions") { id =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"list-${resourceMethods.name}-revisions"
            standardRouteHandling(actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_START.?,
                QUERY_LIMIT.as[Int] ? QUERY_LIMIT_DEFAULT
              ) { (start: Option[String], limit: Int) =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++ Seq(QUERY_START, QUERY_LIMIT))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    onComplete {
                      resourceMethods.resourceMethods.getAllRevisions(
                        id,
                        ignoreBlank(identity, start, QUERY_START),
                        Some(limit)
                      )
                    } {
                      case Success(ers) =>
                        handleSuccess(
                          container = container,
                          status = StatusCodes.OK,
                          json = Some(ers),
                          assetId = None,
                          instance = instance,
                          assetName = None,
                          params = params,
                          actionType = ReportingAgent.search,
                          actionName = actionName,
                          assetType = resourceMethods.name
                        )
                      case Failure(exception) =>
                        handleFailure(
                          container = container,
                          exception = exception,
                          assetId = None,
                          instance = instance,
                          assetName = None,
                          actionType = ReportingAgent.search,
                          actionName = actionName,
                          assetType = resourceMethods.name
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

  def createRevision: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "revisions") { id =>
        post {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"create-${resourceMethods.name}"
            standardRouteHandling(actionName, requiresSpaceOrProjectId = false, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, _, _) =>
              checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = false, resourceMethods.spaceOnly))(ctx, identity)
              entity(as[JsValue]) {
                jsonEntity => {
                  if ((jsonEntity == null) || (jsonEntity == JsNull))
                    throw ServiceException(StatusCodes.BadRequest, MissingJsonDocumentMessage())
                  val container: Container = getContainerFromRequestEntity(jsonEntity, spaceOnly = resourceMethods.spaceOnly)
                  authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                    callContext(versionDate, container) { implicit callContext =>
                      onComplete {
                        resourceMethods.resourceMethods.createRevision(id, jsonEntity)
                      } {
                        case Success((er, sc)) =>
                          handleSuccess(
                            container = container,
                            status = sc,
                            json = Some(er),
                            assetId = None,
                            instance = instance,
                            assetName = None,
                            params = params,
                            actionType = ReportingAgent.create,
                            actionName = actionName,
                            assetType = resourceMethods.name
                          )
                        case Failure(exception) =>
                          handleFailure(
                            container = container,
                            exception = exception,
                            assetId = None,
                            instance = instance,
                            assetName = None,
                            actionType = ReportingAgent.create,
                            actionName = actionName,
                            assetType = resourceMethods.name
                          )
                      }
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
