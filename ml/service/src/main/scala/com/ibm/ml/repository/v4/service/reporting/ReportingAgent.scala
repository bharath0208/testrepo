/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.reporting

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, StatusCode, StatusCodes}
import com.ibm.analytics.wml.service.utils.activitytracker.ActivityTrackerEvent
import com.ibm.analytics.wml.service.utils.activitytracker.ActivityTrackerEvent._
import com.ibm.analytics.wml.service.utils.security.model.Subject.User
import com.ibm.analytics.wml.service.utils.security.model.{Identity, ServiceInstance}
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.clients.instrumentation.impl.segment.Errors.SegmentConfigMissingException
import com.ibm.analytics.wml.utils.clients.instrumentation.impl.segment.SegmentSender
import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}
import com.ibm.ml.repository.v4.service.reporting.ReportingAgent.{RequestData, activityTrackerLogger}
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging._
import com.typesafe.scalalogging.{Logger, StrictLogging}
import org.slf4j.LoggerFactory
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/**
 * Documentation:
 *
 * CADF events (for activity tracker): https://test.cloud.ibm.com/docs/observability?topic=observability-at_event_ov
 * Segment: https://segment-standards.prod.ddp.cis.ibm.net/domains/instrumentation
 *
 * Add rest call instrumentation accordingly to https://github.ibm.com/NGP-TWC/ml-event-consumer/wiki
 * and activity tracker https://test.cloud.ibm.com/docs/services/Activity-Tracker-with-LogDNA?topic=logdnaat-event#event
 */
object ReportingAgent {
  private val activityTrackerLogger: Logger = Logger(LoggerFactory.getLogger(s"${getClass.getPackage.getName}.ActivityTracker"))

  sealed trait ActionType {
    val name: String

    override def toString: String = name
  }

  case object create extends ActionType {
    override val name: String = "create"
  }

  case object delete extends ActionType {
    override val name: String = "delete"
  }

  case object update extends ActionType {
    override val name: String = "update"
  }

  case object read extends ActionType {
    override val name: String = "read"
  }

  case object search extends ActionType {
    override val name: String = "search"
  }

  case object upload extends ActionType {
    override val name: String = "upload"
  }

  case object download extends ActionType {
    override val name: String = "download"
  }

  def actionType(name: String): Option[ActionType] = {
    name match {
      case "create" => Some(ReportingAgent.create)
      case "delete" => Some(ReportingAgent.delete)
      case "update" => Some(ReportingAgent.update)
      case "read" => Some(ReportingAgent.read)
      case "search" => Some(ReportingAgent.search)
      case "upload" => Some(ReportingAgent.upload)
      case "download" => Some(ReportingAgent.download)
      case _ => None
    }
  }

  case class RequestData(projectId: Option[String],
                         spaceId: Option[String],
                         requestId: Option[String],
                         assetId: String,
                         assetType: String)

  object RequestData {
    def apply(identity: Identity,
              container: Container,
              state: MLRepositoryState): RequestData = {
      val (projectId, spaceId) = container match {
        case Project(id) => (Some(id), None)
        case Space(id) => (None, Some(id))
      }
      RequestData(
        projectId = projectId,
        spaceId = spaceId,
        requestId = identity.requestId,
        assetId = state.assetId,
        assetType = state.assetType
      )
    }
  }

  private def getActionTypeName(actionType: ActionType): String = {
    actionType match {
      case ReportingAgent.search =>
        "list"
      case ReportingAgent.upload =>
        "add"
      case ReportingAgent.download =>
        "read"
      case other =>
        other.toString
    }
  }

  def createAction(state: MLRepositoryState,
                   serviceName: String): String = {
    s"$serviceName.${state.assetType}.${getActionTypeName(state.actionType)}"
  }

  def createTargetTypeURI(state: MLRepositoryState,
                          serviceName: String): String = {
    // https://test.cloud.ibm.com/docs/observability?topic=observability-at_req_fields#target.typeURI
    s"$serviceName/${state.assetType}"
  }

  def getServiceHost: Option[Host] = {
    wmlPublicHost match {
      case Success(host) =>
        Some(Host(host, None))
      case Failure(_) =>
        None
    }
  }

  def getSeverity(actionType: ActionType,
                  status: StatusCode): Severity = {
    /*
     * Value      Type of action                                                                                       Sample of action
     * -------------------------------------------------------------------------------------------------------------------------------------------
     * normal     Routine actions in the Cloud                                                                         Start an instance
     * warning    Actions that fail
     *            Actions where a resource is updated or its metadata is modified                                      Rename a service instance
     * critical   Actions that affect security in the Cloud such as changing credentials of a user or deleting data
     *            Actions where the initiator is not authorized to work with a Cloud resource                          Delete a security key
     */
    if (status.isSuccess()) {
      // create, delete, update, read, search, upload, download
      actionType match {
        case ReportingAgent.delete => critical
        case ReportingAgent.update => warning
        case ReportingAgent.upload => warning
        case _ => normal
      }
    } else
      actionType match {
        case ReportingAgent.delete => critical
        case _ => warning
      }
  }

  def getMessage(state: MLRepositoryState): String = {
    // https://test.cloud.ibm.com/docs/observability?topic=observability-at_req_fields#message
    val targetName: String = state.assetName match {
      case Some(name) => s" $name"
      case None => ""
    }
    s"Machine Learning: ${getActionTypeName(state.actionType)} ${state.assetType}$targetName"
  }

  def getTarget(state: MLRepositoryState,
                serviceName: String): Target = {
    Target(
      id = state.assetId,
      name = state.assetName,
      typeURI = ReportingAgent.createTargetTypeURI(state, serviceName),
      host = ReportingAgent.getServiceHost
    )
  }

  implicit val actionTypeFormat: RootJsonFormat[ActionType] = new RootJsonFormat[ActionType]() {
    override def write(obj: ActionType): JsValue = JsString(obj.name)

    override def read(json: JsValue): ActionType = {
      json match {
        case JsString(name) =>
          actionType(name) match {
            case Some(at) => at
            case None =>
              deserializationError(s"No action type found for ${logPrint(json)}")
          }
        case _ => deserializationError(s"No action type found for ${logPrint(json)}")
      }
    }
  }

  implicit val requestDataFormat: RootJsonFormat[RequestData] = jsonFormat5(RequestData.apply)

  /*private val instrumentationActor: Option[ActorRef] = {
    val allowInstrumentation: Boolean = config.hasPath("ml-event-client")
    val name = ContextDefinitions.SYSTEM_EVENTS
    logger.info(s"Creating instrumentation actor with name $name")
    Try(Some(ContextDefinitions.createActorSystem(config, ContextDefinitions.SYSTEM_EVENTS).actorOf(props(new SegmentProvider()), name = name))).getOrElse(None)
    None
  }*/
}

case class ReportingAgent() extends StrictLogging {
  private def shouldSendEvents: Boolean = {
    false // isPublicCloud & !isDevelopment
  }

  private def shouldTrackActivity: Boolean = {
    isPublicCloud // & !isDevelopment
  }

  private def shouldSendSegment: Boolean = {
    val segmentEnabled = "service.ml-repository.segment.enabled"
    config.hasPath(segmentEnabled) && config.getBoolean(segmentEnabled) && isPublicCloud
  }

  private val segmentSender: Option[SegmentSender] = try {
    Some(SegmentSender())
  } catch {
    case t: SegmentConfigMissingException => {
      ExceptionLogger.log(msg = Some("Segment sender configuration missing"), exception = t, requestId = None)
      None
    }
  }

  private def getLogSourceCRN(configuration: ActivityTrackerConfiguration,
                              instance: Option[ServiceInstance],
                              identity: Identity,
                              state: MLRepositoryState): Future[String] = {
    instance match {
      case Some(inst) if inst.crn.isDefined =>
        Future.successful(inst.crn.get)
      case _ =>
        identity.accountId match {
          case Some(id) if identity.subject.subjectType == User =>
            Future.successful(configuration.crn(id, state.assetType, state.assetId))
          case _ =>
            Future.failed(throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage("Failed to find account id for Activity Tracker event")))
        }
    }
  }

  /**
   * This method must NOT throw an exception.
   */
  def reportEvent(identity: Identity,
                  container: Container,
                  instance: Option[ServiceInstance],
                  request: HttpRequest,
                  state: MLRepositoryState,
                  httpResponseCode: Int,
                  patch: Option[JsValue])
                 (implicit system: ActorSystem,
                  builder: HttpClientBuilder): Future[Unit] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val events: Future[Unit] = Future.successful {
      if (shouldSendEvents) {
        Try {
          // TODO ReportingAgent.instrumentationActor.foreach(actor => actor ! RestMirroringMessage(request, identity, state.toJson, patch))
        } match {
          case Success(_) =>
          case Failure(exception) =>
            ExceptionLogger.log(Some("Failed to send REST mirroring event"), exception, identity.requestId)
        }
      }
    }

    val activity: Future[Unit] = {
      if (shouldTrackActivity) {
        {
          for {
            status <- Future.fromTry {
              Try {
                StatusCodes.getForKey(httpResponseCode) match {
                  case Some(code) =>
                    code
                  case None =>
                    throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(s"Failed to find status code for $httpResponseCode"))
                }
              }
            }
            configuration <- Future.fromTry(Try(ActivityTrackerConfiguration()))
            logSourceCRN <- getLogSourceCRN(configuration, instance, identity, state)
            event <- Future.fromTry(
              ActivityTrackerEvent(
                logSourceCRN = logSourceCRN,
                serviceName = configuration.serviceName,
                identity = identity,
                container = container,
                httpResponseCode = httpResponseCode,
                userAgentHttpHeader = getUserAgent(request),
                action = ReportingAgent.createAction(state, serviceName = configuration.serviceName),
                message = ReportingAgent.getMessage(state),
                resourceGroupId = "", // TODO - for now we do this - maybe the AT team will have a better idea
                requestData = Some(RequestData(identity, container, state).toJson.asJsObject),
                responseData = None,
                severity = ReportingAgent.getSeverity(state.actionType, status),
                target = ReportingAgent.getTarget(state, configuration.serviceName),
                eventTime = None, // this will be set to 'now'
                id = None,
                correlationId = identity.requestId
              )
            )
            json <- Future.fromTry(Try(event.toJson.compactPrint))
          } yield {
            activityTrackerLogger.info(json)
          }
        } recover {
          case exception: Exception =>
            ExceptionLogger.log(Some(s"Failed to send the activity tracker event: ${exception.getMessage}"), exception, identity.requestId)
        }
      } else {
        Future.successful(())
      }
    }

    val segment: Future[Unit] = Future.successful {
      if (shouldSendSegment) {
        segmentSender.map(s =>
          try {
            val event = SegmentEvent(
              identity = identity,
              instanceId = if (instance.isDefined) Some(instance.get.instanceId) else None,
              request = request,
              state = state,
              httpResponseCode = httpResponseCode
            )
            reqId(identity.requestId)(() => logger.trace(s"Sending Segment event ${logPrint(event.toJson)}"))
            s.sendTrackMessage(userId = identity.subject.id, properties = event.toJson.convertTo[Map[String, String]], name = "API Call")
          } catch {
            case t: Throwable => ExceptionLogger.log(msg = Some("Fail on sending segment information"), exception = t, requestId = identity.requestId)
          }
        )
      }
    }

    for {
      _ <- events
      _ <- activity
      _ <- segment
    } yield {
    }
  }
}

case class RestMirroringMessage(request: HttpRequest,
                                identity: Identity,
                                resourceState: JsValue,
                                patch: Option[JsValue])

case class MLRepositoryState(assetId: String,
                             assetType: String,
                             assetName: Option[String],
                             actionType: ReportingAgent.ActionType,
                             actionName: String,
                             state: Option[JsValue])

object MLRepositoryState extends DefaultJsonProtocol {
  implicit val mLRepositoryStateFormat: RootJsonFormat[MLRepositoryState] = jsonFormat6(MLRepositoryState.apply)
}
