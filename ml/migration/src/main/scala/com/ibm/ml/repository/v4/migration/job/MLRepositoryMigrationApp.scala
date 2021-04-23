/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.job

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.service.utils.security.iam.{IAM, IAMContext}
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.analytics.wml.utils.security.{ConfigDecrypter, WMLInstanceHttpClient}
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.models.{Failed, MigrationDoc, OldInstance}
import com.ibm.ml.repository.v4.migration.service.MLRepositoryMigrationServer
import com.ibm.ml.repository.v4.migration.utils.v4beta.repository.V4BetaRepositoryClient
import com.ibm.ml.repository.v4.migration.utils.{MLRepositoryMigrationAppFailed, MigrationConstant}
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.ibm.ml.repository.v4.utils.{FailedToConvertMigrationJob, MissingRequiredEnvironmentVariable, ServiceException, _}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MLRepositoryMigrationApp(args: Array[String]) extends ConfigDecrypter with MigrationConstant with StrictLogging {
  private val specificationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationServer]("Specification-Version")
  private val implementationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationServer]("Implementation-Version")
  logger.info(s"Initialize ML Repository Migration App Context with App Implementation Version: $implementationVersion and Specification Version: $specificationVersion")

  private def jobDetailsFromArgs(args: Array[String]): Option[String] = {
    args.find(_.startsWith(JOB_DETAILS)).map(_.split("=").last)
  }

  private lazy val jobDetailsEncoded: String = sys.env.getOrElse(JOB_DETAILS, jobDetailsFromArgs(args).getOrElse {
    throw ServiceException(StatusCodes.InternalServerError, MissingRequiredEnvironmentVariable(JOB_DETAILS))
  })

  val jobDetails: MigrationDoc = base64Decode(jobDetailsEncoded) match {
    case Success(details) =>
      Try(details.parseJson.convertTo[MigrationDoc]) match {
        case Success(job) =>
          job
        case Failure(_) =>
          throw ServiceException(StatusCodes.InternalServerError, FailedToConvertMigrationJob(details))
      }
    case Failure(_) =>
      throw ServiceException(StatusCodes.InternalServerError, MissingRequiredEnvironmentVariable(JOB_DETAILS))
  }

  private val publicWmlHost: Try[String] = {
    Try(config.getString("service.wml.host")) match {
      case Success(host) =>
        Success(host)
      case Failure(exception) =>
        Failure(exception)
    }
  }

  private val publicWmlPort: Int = {
    val DEFAULT_PORT = 443
    Try(config.getInt("service.wml.port")) match {
      case Success(port) =>
        port
      case Failure(_) =>
        logger.info(s"wml port using default value $DEFAULT_PORT")
        DEFAULT_PORT
    }
  }

  private def getWmlHostAndPort(context: MigrationAppContext,
                                oldInstance: OldInstance): (String, Int) = {
    // we use private endpoints if we are not using a users API key
    if (oldInstance.apiKey.isEmpty)
      (wmlHost.get, wmlPort)
    else {
      // we could just return the public endpoints and hope that it works...
      implicit val system: ActorSystem = context.downstreamActorSystem
      implicit val httpClientLoader: HttpClientBuilder = context.authHttp
      implicit val iam: IAMContext = IAMContext()
      implicit val ec: ExecutionContext = system.dispatcher


      val f = for {
        token <- IAM.getFromApiKey(oldInstance.apiKey.get)
        details <- WMLInstanceHttpClient().getInstanceDetails(oldInstance.instanceId, token)
      } yield {
        if (!details.service_endpoints.toLowerCase.contains("private"))
          publicWmlHost match {
            case Success(host) =>
              (host, publicWmlPort)
            case Failure(exception) =>
              logger.error(s"Failed to find public WML host, defaulting to private endpoints: ${exception.getMessage}")
              (wmlHost.get, wmlPort)
          }
        else // the user has specified private or public_private
          (wmlHost.get, wmlPort)
      }
      Try(Await.result(f, 2.minutes)) match {
        case Success((host, port)) =>
          (host, port)
        case Failure(exception) =>
          publicWmlHost match {
            case Success(host) =>
              logger.error(s"Failed to find service instance for ${oldInstance.instanceId}, defaulting to public endpoints: ${exception.getMessage}")
              (host, publicWmlPort)
            case Failure(exception) =>
              logger.error(s"Failed to find service instance for ${oldInstance.instanceId}, defaulting to private endpoints: ${exception.getMessage}")
              (wmlHost.get, wmlPort)
          }
      }
    }
  }

  // we create an instance rather than using inheritance so that this
  // class is created before the context
  private class Ctxt() extends MigrationAppContext {
    override val v4BetaRepositoryClient: V4BetaRepositoryClient = {
      val (host, port) = getWmlHostAndPort(this, jobDetails.oldInstance)
      V4BetaRepositoryClient.createV4BetaRepositoryClient(
        host,
        port,
        jobDetails.oldInstance
      )(
        v4RepoHttp,
        downstreamActorSystem,
        iamContext
      )
    }
  }


  implicit val ctxt: MigrationAppContext = {
    Try {
      new Ctxt()
    } match {
      case Success(context) =>
        context
      case Failure(exception) =>
        ExceptionLogger.log(Some("Failed to initialize ml-repository App context"), exception, None, callStack = true)
        throw utils.ServiceException(
          StatusCodes.InternalServerError,
          MLRepositoryMigrationAppFailed()
        )
    }
  }


  def start(): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher

    sys.addShutdownHook(system.terminate())

    val migrationId = jobDetails.id.getOrElse(
      throw ServiceException(StatusCodes.InternalServerError, MissingRequiredEnvironmentVariable(MIGRATION_JOB_ID))
    )

    (for {
      // read job info from db
      migrationDoc <- ctxt.dbMethods.getMigrationDoc(migrationId)
      userId = migrationDoc.userId
      identity <- ctxt.getIdentity(userId)
      softwareSpecs <- ctxt.environmentsClient.getSoftwareSpecsResources(identity,
        None,
        container = Some(getContainer(migrationDoc.spaceId, migrationDoc.projectId)))
      hardwareSpecs <- ctxt.environmentsClient.getHardwareSpecsResources(identity,
        None,
        container = Some(getContainer(migrationDoc.spaceId, migrationDoc.projectId)))
      _ <- MigrationJob(ctxt, migrationDoc, softwareSpecs, hardwareSpecs)(identity).start()
    } yield {}) recoverWith {
      case t: Throwable =>
        val msg = s"Migration job failed due to ${t.getMessage}"
        for {
          doc <- ctxt.dbMethods.getMigrationDoc(migrationId)
          _ <- ctxt.dbMethods.updateStatus(Failed, doc.addMessageToConsole(msg))
        } yield {
          throw t
        }
    }

  }
}

object MLRepositoryMigrationJobApp extends App with StrictLogging {
  private val JOB_CUSTOM_METRIC = "Custom/V4CloudMigration"

  def jobDuration: Float = {
    val duration: Long = System.currentTimeMillis() - now
    duration.toFloat
  }

  val now = System.currentTimeMillis()
  Try {
    Await.result(MLRepositoryMigrationApp(args).start(), 24.hours)
  } match {
    case Success(_) =>
      logger.info("Job terminated normally")
      System.exit(0)
    case Failure(exception) =>
      ExceptionLogger.log(Some("Job terminated with an exception"), exception, None, callStack = true)
      System.exit(1)
  }
}
