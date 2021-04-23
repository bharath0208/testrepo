/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.service

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpRequest, StatusCodes}
import akka.http.scaladsl.server.Directives.{extractRequest, handleExceptions, handleRejections, mapRejections, mapResponse, respondWithDefaultHeaders}
import akka.http.scaladsl.server.{ExceptionHandler, RejectionHandler, Route, RouteConcatenation}
import akka.http.scaladsl.{ConnectionContext, Http}
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.ml.repository.v4.migration.api.internal.HeartbeatAPI
import com.ibm.ml.repository.v4.migration.api.v4.MigrationV4API
import com.ibm.ml.repository.v4.migration.utils.{EndpointCreationFailed, MLRepositoryMigrationServiceFailed}
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.errors.{ServiceExceptionHandler, ServiceRejectionHandler}
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging

import scala.util.{Failure, Success, Try}

case class MLRepositoryMigrationServer(args: Array[String]) extends ServiceUtils with APIUtils with StrictLogging {
  private val specificationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationServer]("Specification-Version")
  private val implementationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationServer]("Implementation-Version")

  // we create an instance rather than using inheritance so that this
  // class get's created before the context
  private class Ctxt() extends MigrationServiceContext

  implicit val sc: MigrationServiceContext = {
    Try {
      new Ctxt()
    } match {
      case Success(context) =>
        context
      case Failure(exception) =>
        ExceptionLogger.log(Some("Failed to initialize ml-repository migration service context"), exception, None, callStack = true)
        throw ServiceException(
          StatusCodes.InternalServerError,
          MLRepositoryMigrationServiceFailed()
        )
    }
  }

  val rejectionHandler: RejectionHandler = ServiceRejectionHandler()
  val exceptionHandler: ExceptionHandler = ServiceExceptionHandler()

  private def traceRequest(request: HttpRequest,
                           endpoints: Route): Route = {

    import RouteConcatenation._
    val requestStarted = System.currentTimeMillis()
    handleRejections(rejectionHandler) {
      handleExceptions(exceptionHandler) {
        logRequest(request)
        mapResponse(logResponse(request, requestStarted)) {
          mapRejections(logRejections(request, requestStarted)) {
            respondWithDefaultHeaders(getRequestIdHeadersForResponse(request)) {
              HeartbeatAPI(sc)(specificationVersion getOrElse "?", implementationVersion getOrElse "?").endpoints ~
                respondWithDefaultHeaders(getResponseHeaders(request)) {
                  endpoints
                }
            }
          }
        }
      }
    }
  }

  private def getEndpoints: Route = {
    val endpoints: Route = Try(MigrationV4API(sc).endpoints) match {
      case Success(endpoints) => endpoints
      case Failure(exception) =>
        logger.error(s"Failed to initialize the endpoints: ${ServiceException.getExceptionMessage(exception)}", exception)
        throw utils.ServiceException(
          StatusCodes.InternalServerError,
          EndpointCreationFailed()
        )
    }
    extractRequest { request =>
      traceRequest(request, endpoints)
    }
  }

  logger.debug(s"Initializing ml-migration-repository endpoints...")
  val endpoints: Route = getEndpoints
  logger.info(s"Initialized ml-migration-repository endpoints: API=${specificationVersion.get} Build=${implementationVersion.get}")

  def start(): Unit = {
    implicit val system: ActorSystem = sc.endpointsActorSystem

    sys.addShutdownHook(system.terminate())

    val httpHost: String = {
      val useLocalhost = Try {
        config.getBoolean("service.ml-repository.http.use-localhost-in-dev")
      } match {
        case Success(v) if isDevelopment => v
        case _ => false
      }
      if (useLocalhost && isDevelopment)
        "localhost"
      else {
        Try(config.getString("akka.http.service.interface")) match {
          case Success(address) if address != "auto" => address
          case _ => getLocalHostName
        }
      }
    }

    val jre: String = System.getProperty("java.version") // "java.vm.version"

    if (useSsl) {
      val sslPort = Try(config.getInt("service.ssl.port")).getOrElse(443)
      Http().newServerAt(httpHost, sslPort).enableHttps(ConnectionContext.httpsServer(getSSLContext(sc))).bind(endpoints)
      logger.info(s"Server started at https://$httpHost:$sslPort (jre $jre)")
    } else {
      // we should never get here
      throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage("Failed to create the SSL context"))
    }
  }
}

object MLRepositoryMigrationServerApp extends App with StrictLogging {
  Try {
    MLRepositoryMigrationServer(args).start()
  } match {
    case Success(_) =>
      logger.info("Service terminated normally")
    case Failure(exception) =>
      logger.error(s"Service terminated with an exception: ${exception.getMessage}", exception)
      System.exit(1)
  }
}
