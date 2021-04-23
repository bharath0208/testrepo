/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.cams

import akka.actor.ActorSystem
import com.ibm.analytics.wml.service.utils.security.iam.IAMContext
import com.ibm.analytics.wml.utils.clients.http._
import com.ibm.ml.repository.v4.utils.{ContextDefinitions, _}
import com.ibm.ml.repository.v4.utils.logging._
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object GlobalAssetTypesApp extends App with StrictLogging {
  private val appTimeout = getDuration("register-global-asset-types.timeout", 24.hours)

  private val system: ActorSystem = ContextDefinitions.createActorSystem(
    config = config,
    systemName = ContextDefinitions.SYSTEM_CAMS_GLOBAL_ASSETS
  )
  // require(system.dispatcher != null)

  private def retryPolicy: RetryPolicy = RetryFailureTimeCapped(
    numRetries = Try(config.getInt("register-global-asset-types.retry.num-retries")).getOrElse(3),
    maxRequestTime = getDuration(name = "register-global-asset-types.retry.max-request-time", fallback = 3.minutes).toMillis.toInt,
    delay = getDuration(name = "register-global-asset-types.retry.delay", fallback = 1.second).toMillis.toInt
  )

  private def appClientLoader(commands: CommandLine): HttpClientBuilder = {
    def reportingFunc(metric: HttpMetric): Unit = {
      def toString(metric: HttpMetric): String = {
        val report = JsObject(
          Map(
            "host" -> JsString(metric.host),
            "port" -> JsNumber(metric.port),
            "method" -> JsString(metric.method),
            "uri" -> JsString(metric.uri),
            "startTime" -> JsString(formatAsDate(metric.startTime))
          ) ++ (if (metric.statusCode.isDefined) Map("statusCode" -> JsNumber(metric.statusCode.get)) else Map())
            ++ (if (metric.duration > 0) Map("duration" -> JsNumber(metric.duration)) else Map())
            ++ (if (metric.action.isDefined) Map("action" -> JsString(metric.action.get)) else Map())
            ++ (if (metric.origin.isDefined) Map("origin" -> JsString(metric.origin.get)) else Map())
            ++ (if (metric.service.isDefined) Map("service" -> JsString(metric.service.get)) else Map())
        )
        logPrint(js = report, prettyPrint = Some(false))
      }

      reqId(metric.contextId)(() => logger.info(toString(metric)))
    }

    CachedHttpClientBuilder(
      allowAllCerts = commands.isICP, // if ICP then allow all certs
      service = Some(serviceName),
      reporting = reportingFunc
    )(system)
  }

  def commandName: String = {
    val name = GlobalAssetTypesApp.getClass.getSimpleName
    if (name.endsWith("$"))
      name.substring(0, name.length - 1)
    else
      name
  }

  def serviceName: String = {
    val name = GlobalAssetTypesCreator.getClass.getSimpleName
    if (name.endsWith("$"))
      name.substring(0, name.length - 1)
    else
      name
  }

  // this can fail in which case an exception is thrown
  val commands: CommandLine = Try(CommandLine(args)) match {
    case Success(commands) =>
      logger.info(s"Running command: $commands")
      commands
    case Failure(exception) =>
      logger.error(exception.getMessage)
      logger.info(CommandLine.usage())
      throw exception
  }

  if (commands.usageOnly) {
    logger.info(CommandLine.usage())
  } else {
    val clientLoader: HttpClientBuilder = appClientLoader(commands)
    // register the global types and wait for the execution to finish
    Try(
      Await.result(
        declareGlobalAssetTypes(
          serviceUrl = commands.serviceUrl,
          listOnly = commands.listOnly,
          forceUpdate = commands.forceUpdate,
          authentication = CAMSAuthenticator.getAuthDetails(commands, clientLoader, retryPolicy)(system, IAMContext()),
          clientLoader = clientLoader,
          retryPolicy = retryPolicy,
          system = system,
          requestId = commands.requestId
        ),
        appTimeout
      )
    ) match {
      case Success(status) =>
        logger.info(s"Completed: ${status.toJson.prettyPrint}")
        if (!commands.noExit)
          System.exit(0)
      case Failure(exception) =>
        logger.error(s"Failed: ${exception.getMessage}", exception)
        if (!commands.noExit)
          System.exit(1)
        else
          throw exception
    }
  }

  def declareGlobalAssetTypes(serviceUrl: String,
                              listOnly: Boolean,
                              forceUpdate: Boolean,
                              authentication: Future[Map[String, String]],
                              clientLoader: HttpClientBuilder,
                              retryPolicy: RetryPolicy,
                              system: ActorSystem,
                              requestId: Option[String]): Future[List[AssetStatus]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    for {
      auth <- authentication
      responses <- GlobalAssetTypesCreator(
        serviceUrl,
        auth,
        listOnly,
        forceUpdate,
        clientLoader,
        retryPolicy
      ).declareGlobalAssetTypes(requestId)(system)
    } yield {
      responses
    }
  }
}
