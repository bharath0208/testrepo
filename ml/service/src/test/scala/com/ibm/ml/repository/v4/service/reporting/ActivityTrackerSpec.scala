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
import akka.http.scaladsl.model.{RemoteAddress, StatusCodes}
import com.ibm.analytics.wml.service.utils.activitytracker.ActivityTrackerEvent
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableServiceId, IAMTokenValidator}
import com.ibm.analytics.wml.service.utils.security.model.Identity.IAM
import com.ibm.analytics.wml.service.utils.security.model.Subject.User
import com.ibm.analytics.wml.service.utils.security.model.{Identity, Subject}
import com.ibm.analytics.wml.utils.clients.http.{CachedHttpClientBuilder, HttpClientBuilder}
import com.ibm.analytics.wml.utils.containers.{Container, Space}
import com.ibm.ml.repository.v4.service.reporting.ReportingAgent.RequestData
import com.ibm.ml.repository.v4.service.utils.AssetConstant
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import java.net.InetAddress
import java.util.UUID
import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class ActivityTrackerSpec extends AnyFeatureSpec with AssetConstant with StrictLogging {
  private implicit val system: ActorSystem = ActorSystem("activity-tracker-spec-system")
  private implicit val builder: HttpClientBuilder = CachedHttpClientBuilder()
  private implicit val iamContext: IAMContext = IAMContext()

  private lazy val serviceToken: Try[String] = Try(Await.result(IAMStableServiceId().createServiceIdToken(), 1.minute))
  // quick fix to remove extra quote will try to fix from root later
  private lazy val apiKey = sys.env("WML_API_KEY").replaceAll("^\"|\"$", "")
  private lazy val userToken: Try[String] = Try(Await.result(com.ibm.analytics.wml.service.utils.security.iam.IAM.getFromApiKey(apiKey), 1.minute))

  private val atConfig = ActivityTrackerConfiguration(
    serviceProviderName = "staging",
    serviceProviderType = "public",
    serviceName = "pm-20-stage",
    serviceProviderRegion = "us-south"
  )

  // these are used to create example events etc
  private def logSourceCRN(state: MLRepositoryState) = atConfig.crn("<accountId>", state.assetType, state.assetId)

  private val identity: Identity = Identity(
    subject = Subject(
      subjectType = User,
      id = "<user_id>",
      name = Some("<optional_name>"),
      email = Some("<optional_email>")
    ),
    rawToken = "<raw_token>",
    realm = IAM,
    accountId = Some("<account_id>"),
    requestId = Some(UUID.randomUUID().toString),
    clientIP = Some(RemoteAddress(InetAddress.getLocalHost))
  )
  private val serviceName = "pm-20"
  private val container: Container = Space("<space_id>")

  private val actions: Seq[Array[String]] = Seq(
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.search.name),
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.create.name),
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.delete.name),
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.update.name),
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.read.name),
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.upload.name),
    Array("200", WML_MODEL_ASSET_TYPE, ReportingAgent.download.name)
  )

  Feature("ActivityTracker") {
    Scenario("check event format") {
      val token: Try[String] = if (userToken.isSuccess) userToken else if (serviceToken.isSuccess) serviceToken else Failure(new Exception("Test disabled as no API key or service id found"))
      token match {
        case Success(_) =>
          val identity: Identity = Await.result(IAMTokenValidator().validate(token.get), 1.minute)
          val container = Space("id")
          val state = MLRepositoryState(
            assetId = "asset-id",
            assetType = "model",
            assetName = None,
            actionType = ReportingAgent.create,
            actionName = "create-model",
            state = None
          )
          val events: Seq[Try[ActivityTrackerEvent]] = Seq(
            ActivityTrackerEvent(
              logSourceCRN = logSourceCRN(state),
              serviceName = serviceName,
              identity = identity,
              container = container,
              httpResponseCode = 200,
              userAgentHttpHeader = Some("dummy-user-agent"),
              action = ReportingAgent.createAction(state = state, serviceName = serviceName),
              message = ReportingAgent.getMessage(state),
              resourceGroupId = "",
              requestData = Some(RequestData(identity, container, state).toJson.asJsObject),
              responseData = None,
              severity = ActivityTrackerEvent.normal,
              target = ReportingAgent.getTarget(state, serviceName),
              eventTime = None,
              id = None,
              correlationId = identity.requestId
            )
          )
          for (event <- events) {
            info(s"Event : \n${event.get.toJson.prettyPrint}")
          }
        case Failure(exception) =>
          info(exception.getMessage)
      }
    }

    Scenario("create all events") {
      for (action <- actions) {
        val httpResponseCode = action(0).toInt
        val state = MLRepositoryState(
          assetId = "<asset_id>",
          assetType = action(1),
          assetName = Some("<optional_asset_name>"),
          actionType = ReportingAgent.actionType(action(2)).get,
          actionName = "unused",
          state = None
        )
        val event = ActivityTrackerEvent(
          logSourceCRN = logSourceCRN(state),
          serviceName = serviceName,
          identity = identity,
          container = container,
          httpResponseCode = httpResponseCode,
          userAgentHttpHeader = Some("dummy-user-agent"),
          action = ReportingAgent.createAction(state = state, serviceName = serviceName),
          message = ReportingAgent.getMessage(state),
          resourceGroupId = "",
          requestData = Some(RequestData(identity, container, state).toJson.asJsObject),
          responseData = None,
          severity = ActivityTrackerEvent.normal,
          target = ReportingAgent.getTarget(state, serviceName),
          eventTime = None,
          id = None,
          correlationId = identity.requestId
        )
        event match {
          case Success(ev) =>
            info(s"Event: ${ev.toJson.prettyPrint}")
          case Failure(exception) =>
            fail(s"Failed to create AT event: ${exception.getMessage}", exception)
        }
      }
    }
    Scenario("create event table") {
      val sb = new StringBuilder()
      sb.append("\n| Action | target.typeURI | Severity OK | Severity Failure |\n")
      sb.append("| :----- | :------------: | :---------: | :--------------: |\n")
      for (action <- actions) {
        val httpResponseCode = action(0).toInt
        val state = MLRepositoryState(
          assetId = "<asset_id>",
          assetType = action(1),
          assetName = Some("<optional_asset_name>"),
          actionType = ReportingAgent.actionType(action(2)).get,
          actionName = "unused",
          state = None
        )
        val event = ActivityTrackerEvent(
          logSourceCRN = logSourceCRN(state),
          serviceName = serviceName,
          identity = identity,
          container = container,
          httpResponseCode = httpResponseCode,
          userAgentHttpHeader = Some("dummy-user-agent"),
          action = ReportingAgent.createAction(state = state, serviceName = serviceName),
          message = ReportingAgent.getMessage(state),
          resourceGroupId = "",
          requestData = Some(RequestData(identity, container, state).toJson.asJsObject),
          responseData = None,
          severity = ActivityTrackerEvent.normal,
          target = ReportingAgent.getTarget(state, serviceName),
          eventTime = None,
          id = None,
          correlationId = identity.requestId
        )
        event match {
          case Success(ev) =>
            sb.append("| ")
            sb.append(ev.action.replace(WML_MODEL_ASSET_TYPE, "&lt;asset type&gt;"))
            sb.append(" | ")
            sb.append(ev.target.typeURI)
            sb.append(" | ")
            sb.append(ReportingAgent.getSeverity(state.actionType, StatusCodes.OK))
            sb.append(" | ")
            sb.append(ReportingAgent.getSeverity(state.actionType, StatusCodes.NotFound))
            sb.append(" |\n")
          case Failure(exception) =>
            fail(s"Failed to create AT event: ${exception.getMessage}", exception)
        }
      }
      info(sb.toString())
    }
  }
}
