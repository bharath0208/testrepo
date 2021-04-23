/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.app

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives.{extractRequest, handleExceptions, handleRejections, mapRejections, mapResponse, respondWithDefaultHeaders}
import akka.http.scaladsl.server._
import akka.http.scaladsl.{ConnectionContext, Http}
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.ml.repository.v4.service.api.internal.{CAMSRegistrationAPI, HeartbeatAPI, ServiceVersionAPI}
import com.ibm.ml.repository.v4.service.api.v4.query.QueryAPI
import com.ibm.ml.repository.v4.service.resources.ResourceRegistry
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.errors.{ServiceExceptionHandler, ServiceRejectionHandler}
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging

import java.net.InetAddress
import scala.util.{Failure, Success, Try}

case class MLRepositoryServer(args: Array[String]) extends ServiceUtils with APIUtils with StrictLogging {
  private val specificationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryServer]("Specification-Version")
  private val implementationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryServer]("Implementation-Version")

  // we create an instance rather than using inheritance so
  // that this class gets created before the context
  private class PrivateContext() extends ServiceContext

  implicit val sc: ServiceContext = {
    Try {
      new PrivateContext()
    } match {
      case Success(context) =>
        context
      case Failure(exception) =>
        ExceptionLogger.log(Some("Failed to initialize ml-repository service context"), exception, None, callStack = true)
        throw ServiceException(
          StatusCodes.InternalServerError,
          MLRepositoryServiceFailed()
        )
    }
  }

  if (isPublicCloud && sc.serviceId.isEmpty && !isDevelopment) {
    val exception = ServiceException(
      StatusCodes.InternalServerError,
      InvalidConfigurationMessage("Failed to initialize WML service id when running in public cloud")
    )
    ExceptionLogger.log(None, exception, None, callStack = true)
    throw exception
  }

  logger.debug(s"Initializing ml-repository endpoints...")
  val rejectionHandler: RejectionHandler = ServiceRejectionHandler()
  val exceptionHandler: ExceptionHandler = ServiceExceptionHandler()
  val serviceEndpoints: Route = getEndpoints
  logger.info(s"Initialized ml-repository endpoints: API=${specificationVersion.get} Build=${implementationVersion.get}")

  private def getEndpoints: Route = {

    val endpoints = ResourceRegistry.getEndpoints(sc)(sc.downstreamActorSystem)
    logger.info(s"Registered routes for ${endpoints.map(e => s"${e.name}s").mkString(",")}")
    val resourceEndpoints: Route = RouteConcatenation.concat(endpoints.map(e => e.endpoints): _*)
    handleRejections(rejectionHandler) {
      handleExceptions(exceptionHandler) {
        extractRequest { request =>
          import RouteConcatenation._
          val requestStarted = System.currentTimeMillis()
          logRequest(request)
          mapResponse(logResponse(request, requestStarted)) {
            mapRejections(logRejections(request, requestStarted)) {
              respondWithDefaultHeaders(getRequestIdHeadersForResponse(request)) {
                HeartbeatAPI(sc)(specificationVersion getOrElse "?", implementationVersion getOrElse "?").endpoints ~
                  ServiceVersionAPI(sc).endpoints ~
                  QueryAPI(sc).endpoints ~
                  CAMSRegistrationAPI(sc).endpoints ~
                  respondWithDefaultHeaders(getResponseHeaders(request)) {
                    resourceEndpoints
                  }
              }
            }
          }
        }
      }
    }

  }

  private val SIGNAL_INT: String = "INT"
  private val SIGNAL_CONT: String = "CONT"

  private def setupQuiesceHandling(): Unit = {
    if (isPrivateCloud) {
      import sun.misc.{Signal, SignalHandler}

      val handleQuiesceStartSignalHandler = new SignalHandler() {
        def handle(sig: Signal): Unit = {
          if (SIGNAL_INT.equalsIgnoreCase(sig.getName.trim())) {
            logger.info(s"Signal $SIGNAL_INT (Quiesce) received")
            sc.startQuiesce()
          }
        }
      }

      val handleQuiesceEndSignalHandler = new SignalHandler() {
        def handle(sig: Signal): Unit = {
          if (SIGNAL_CONT.equalsIgnoreCase(sig.getName.trim())) {
            logger.info(s"Signal $SIGNAL_CONT (Un-Quiesce) received")
            sc.stopQuiesce()
          }
        }
      }

      logger.info(s"Creating signal handlers for Quiesce ($SIGNAL_INT to start and $SIGNAL_CONT to end)")
      Signal.handle(new Signal(SIGNAL_INT), handleQuiesceStartSignalHandler)
      Signal.handle(new Signal(SIGNAL_CONT), handleQuiesceEndSignalHandler)
    }
  }

  def start(): Unit = {
    implicit val system: ActorSystem = sc.endpointsActorSystem

    sys.addShutdownHook(system.terminate())

    setupQuiesceHandling()

    def httpHost: String = {
      val useLocalHost = Try {
        config.getBoolean("service.ml-repository.http.use-localhost-in-dev")
      } match {
        case Success(v) if isDevelopment => v
        case _ => false
      }
      if (useLocalHost && isDevelopment) {
        // check to see if we have "localhost-private"
        if (Try(InetAddress.getAllByName("localhost-private")).getOrElse(Array()).nonEmpty)
          "localhost-private"
        else
          "localhost"
      } else {
        Try(config.getString("akka.http.service.interface")) match {
          case Success(address) if address != "auto" => address
          case _ => getLocalHostName
        }
      }
    }

    val jre: String = System.getProperty("java.version") // "java.vm.version"

    if (useSsl) {
      val sslPort = Try(config.getInt("service.ssl.port")).getOrElse(443)
      Http().newServerAt(httpHost, sslPort).enableHttps(ConnectionContext.httpsServer(getSSLContext(sc))).bind(serviceEndpoints)
      logger.info(s"Server started at https://$httpHost:$sslPort (jre $jre)")
      if (isDevelopment) {
        Try(config.getInt("service.ml-repository.http.port")) match {
          case Success(port) =>
            Http().newServerAt(httpHost, port).bind(serviceEndpoints)
            logger.info(s"Server started at http://$httpHost:$port (jre $jre)")
          case Failure(exception) =>
            logger.error(s"Failed to start the HTTP server because no 'service.ml-repository.http.port' found: ${exception.getMessage}", exception)
        }
      }
    } else {
      // we should never get here
      throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage("Failed to create the SSL context"))
    }
  }
}

object MLRepositoryApp extends App with StrictLogging {
  Try {
    MLRepositoryServer(args).start()
  } match {
    case Success(_) =>
      logger.info("Service terminated normally")
    case Failure(exception) =>
      logger.error(s"Service terminated with an exception: ${exception.getMessage}", exception)
      System.exit(1)
  }
}
