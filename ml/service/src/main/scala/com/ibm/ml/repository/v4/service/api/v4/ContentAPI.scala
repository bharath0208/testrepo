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
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common.ContentMetadata
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
import scala.util.{Failure, Success}

// https://doc.akka.io/docs/akka-http/10.2/routing-dsl/style-guide.html
case class ContentAPI(resourceMethods: ResourceMethods)
                     (implicit sc: ServiceContext) extends AbstractV4API(sc) with AssetConstant with SprayJsonSupport {
  implicit val system: ActorSystem = sc.downstreamActorSystem
  implicit val ec: ExecutionContext = system.dispatcher
  implicit val builder: HttpClientBuilder = sc.authHttp

  def getContent: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "content") { id =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"get-${resourceMethods.name}-content-list"
            standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_REV.?,
                QUERY_CONTENT_FORMAT.?,
                QUERY_NAME.?,
                QUERY_PIPELINE_NODE_ID.?,
                QUERY_DEPLOYMENT_ID.?
              ) { (rev: Option[String], contentFormat: Option[String], name: Option[String], pipelineNodeId: Option[String], deploymentId: Option[String]) =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++
                  Seq(QUERY_REV, QUERY_CONTENT_FORMAT, QUERY_NAME, QUERY_PIPELINE_NODE_ID, QUERY_DEPLOYMENT_ID))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    onComplete {
                      resourceMethods.contentMethods.get.getContent(
                        id,
                        ignoreBlank(identity, rev, QUERY_REV),
                        ignoreBlank(identity, contentFormat.map(_.toLowerCase), QUERY_CONTENT_FORMAT),
                        ignoreBlank(identity, name, QUERY_NAME),
                        ignoreBlank(identity, pipelineNodeId, QUERY_PIPELINE_NODE_ID),
                        ignoreBlank(identity, deploymentId, QUERY_DEPLOYMENT_ID)
                      )
                    } {
                      case Success(ers) =>
                        handleSuccess(
                          container = container,
                          status = StatusCodes.OK,
                          json = Some(ers),
                          assetId = Some(id),
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

  def downloadSingleAttachment: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / resourceMethods.contentAPIName.get) { id =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"download-${resourceMethods.name}-single-attachment"
            standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_REV.?
              ) { rev: Option[String] =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++ Seq(QUERY_REV))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    complete {
                      for {
                        response <- resourceMethods.contentMethods.get.downloadSingleAttachment(
                          id,
                          ignoreBlank(identity, rev, QUERY_REV)
                        )
                        _ <- handleResponse(
                          response = response,
                          container = container,
                          assetId = Some(id),
                          instance = instance,
                          assetName = None,
                          params = params,
                          actionType = ReportingAgent.download,
                          actionName = actionName,
                          assetType = resourceMethods.name
                        )
                      } yield {
                        response
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

  def downloadAttachmentWithFilter: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "download") { id =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"download-${resourceMethods.name}-content-attachment-with-filter"
            standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_REV.?,
                QUERY_CONTENT_FORMAT.?,
                QUERY_NAME.?,
                QUERY_PIPELINE_NODE_ID.?,
                QUERY_DEPLOYMENT_ID.?
              ) { (rev: Option[String], contentFormat: Option[String], name: Option[String], pipelineNodeId: Option[String], deploymentId: Option[String]) =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++
                  Seq(QUERY_REV, QUERY_CONTENT_FORMAT, QUERY_NAME, QUERY_PIPELINE_NODE_ID, QUERY_DEPLOYMENT_ID))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    complete {
                      for {
                        response <- resourceMethods.contentMethods.get.downloadAttachmentWithFilter(
                          id,
                          ignoreBlank(identity, rev, QUERY_REV),
                          ignoreBlank(identity, contentFormat.map(_.toLowerCase), QUERY_CONTENT_FORMAT),
                          ignoreBlank(identity, name, QUERY_NAME),
                          ignoreBlank(identity, pipelineNodeId, QUERY_PIPELINE_NODE_ID),
                          ignoreBlank(identity, deploymentId, QUERY_DEPLOYMENT_ID)
                        )
                        _ <- handleResponse(
                          response = response,
                          container = container,
                          assetId = Some(id),
                          instance = instance,
                          assetName = None,
                          params = params,
                          actionType = ReportingAgent.download,
                          actionName = actionName,
                          assetType = resourceMethods.name
                        )
                      } yield {
                        response
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

  def downloadAttachment: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "content" / Segment) { (id, attachmentId) =>
        get {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"download-${resourceMethods.name}-content-attachment"
            standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              parameters(
                QUERY_REV.?
              ) { rev: Option[String] =>
                checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++ Seq(QUERY_REV))(ctx, identity)
                val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                authorize(actionName, container, AuthorizationAction.Viewer) { instance =>
                  callContext(versionDate, container) { implicit callContext =>
                    complete {
                      for {
                        response <- resourceMethods.contentMethods.get.downloadAttachment(
                          id,
                          attachmentId,
                          ignoreBlank(identity, rev, QUERY_REV)
                        )
                        _ <- handleResponse(
                          response = response,
                          container = container,
                          assetId = Some(id),
                          instance = instance,
                          assetName = None,
                          params = params,
                          actionType = ReportingAgent.download,
                          actionName = actionName,
                          assetType = resourceMethods.name
                        )
                      } yield {
                        response
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

  def deleteAttachment: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "content" / Segment) { (id, attachmentId) =>
        delete {
          authenticate(ctx) { implicit identity =>
            val actionName: String = s"delete-${resourceMethods.name}-content-attachment"
            standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
              checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly))(ctx, identity)
              val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
              authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                callContext(versionDate, container) { implicit callContext =>
                  onComplete {
                    resourceMethods.contentMethods.get.deleteAttachment(id, attachmentId)
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
    }.apply(ctx)
  }

  def uploadAttachment: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / "content") { id =>
        put {
          extractMatchedPath { matched =>
            withSizeLimit(uploadContentMaxLength) {
              extractRequestEntity { re =>
                authenticate(ctx) { implicit identity =>
                  val actionName: String = s"upload-${resourceMethods.name}-content"
                  standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
                    parameters(
                      QUERY_CONTENT_FORMAT,
                      QUERY_NAME.?,
                      QUERY_PIPELINE_NODE_ID.?,
                      QUERY_DEPLOYMENT_ID.?
                    ) { (contentFormat: String, name: Option[String], pipelineNodeId: Option[String], deploymentId: Option[String]) =>
                      val contentType = checkContentTypeHeader(Some(re.contentType.value))
                      val contentFormatLowerCase = rejectBlank(identity, contentFormat.toLowerCase, QUERY_CONTENT_FORMAT)
                      checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly) ++
                        Seq(QUERY_CONTENT_FORMAT, QUERY_NAME, QUERY_PIPELINE_NODE_ID, QUERY_DEPLOYMENT_ID))(ctx, identity)
                      val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                      authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                        callContext(versionDate, container) { implicit callContext =>
                          extractDataBytes { dataSource =>
                            onComplete {
                              resourceMethods.contentMethods.get.uploadAttachment(
                                id,
                                resourceMethods.assetType,
                                dataSource,
                                contentFormatLowerCase,
                                contentType,
                                ignoreBlank(identity, name, QUERY_NAME),
                                ignoreBlank(identity, pipelineNodeId, QUERY_PIPELINE_NODE_ID),
                                ignoreBlank(identity, deploymentId, QUERY_DEPLOYMENT_ID)
                              )
                            } {
                              case Success(result) =>
                                handleSuccess(
                                  container = container,
                                  status = StatusCodes.Created,
                                  json = Some(
                                    ContentMetadata(
                                      attachmentId = result.attachmentId.get,
                                      contentFormat = contentFormatLowerCase,
                                      persisted = true,
                                      name = name,
                                      pipeLineNodeId = pipelineNodeId,
                                      deploymentId = deploymentId
                                    ).toJson
                                  ),
                                  assetId = Some(id),
                                  instance = instance,
                                  assetName = None,
                                  params = params,
                                  actionType = ReportingAgent.upload,
                                  actionName = actionName,
                                  assetType = resourceMethods.name,
                                  headers = List(headers.Location(Uri(s"$matched/${result.attachmentId.get}")))
                                )
                              case Failure(exception) =>
                                handleFailure(
                                  container = container,
                                  exception = exception,
                                  assetId = None,
                                  instance = instance,
                                  assetName = None,
                                  actionType = ReportingAgent.upload,
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
        }
      }
    }.apply(ctx)
  }

  def uploadSingleAttachment: Route = {
    implicit ctx => {
      path("ml" / "v4" / s"${resourceMethods.name}s" / Segment / resourceMethods.contentAPIName.get) { id =>
        put {
          extractMatchedPath { matched =>
            withSizeLimit(uploadContentMaxLength) {
              extractRequestEntity { re =>
                authenticate(ctx) { implicit identity =>
                  val actionName: String = s"upload-${resourceMethods.name}-single-content"
                  standardRouteHandling(actionName = actionName, spaceOnly = resourceMethods.spaceOnly) { (params, versionDate, spaceId, projectId) =>
                    parameters(
                      QUERY_NAME.?
                    ) { name: Option[String] =>
                      val contentType = checkContentTypeHeader(Some(re.contentType.value))
                      checkParameters(params, getStandardHandlingParameters(requiresSpaceOrProjectId = true, resourceMethods.spaceOnly))(ctx, identity)
                      val container: Container = getContainer(spaceId, projectId, spaceOnly = resourceMethods.spaceOnly)
                      authorize(actionName, container, AuthorizationAction.Editor) { instance =>
                        callContext(versionDate, container) { implicit callContext =>
                          extractDataBytes { dataSource =>
                            onComplete {
                              resourceMethods.contentMethods.get.uploadSingleAttachment(
                                id,
                                resourceMethods.assetType,
                                contentType,
                                dataSource,
                                ignoreBlank(identity, name, QUERY_NAME)
                              )
                            } {
                              case Success(result) =>
                                handleSuccess(
                                  container = container,
                                  status = StatusCodes.Created,
                                  json = Some(
                                    ContentMetadata(
                                      attachmentId = result.attachmentId.get,
                                      contentFormat = CONTENT_FORMAT_DEFAULT_VALUE,
                                      persisted = true,
                                      name = name,
                                      pipeLineNodeId = None,
                                      deploymentId = None
                                    ).toJson
                                  ),
                                  assetId = Some(id),
                                  instance = instance,
                                  assetName = None,
                                  params = params,
                                  actionType = ReportingAgent.upload,
                                  actionName = actionName,
                                  assetType = resourceMethods.name,
                                  // in this case we just return the get url (we hide the attachment id)
                                  headers = List(headers.Location(Uri(matched.toString())))
                                )
                              case Failure(exception) =>
                                handleFailure(
                                  container = container,
                                  exception = exception,
                                  assetId = None,
                                  instance = instance,
                                  assetName = None,
                                  actionType = ReportingAgent.upload,
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
        }
      }
    }.apply(ctx)
  }
}
