/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.job

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.containers.Container
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.analytics.wml.utils.security.ConfigDecrypter
import com.ibm.ml.repository.v4.migration.upgrade.models._
import com.ibm.ml.repository.v4.migration.utils.{MLRepositoryMigrationAppFailed, MigrationConstant}
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MLRepositoryDeleteApp(args: Array[String]) extends ConfigDecrypter with MigrationConstant with StrictLogging {
  private val specificationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryDeleteApp]("Specification-Version")
  private val implementationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryDeleteApp]("Implementation-Version")
  logger.info(s"Initialize ML Repository Delete App Context with App Implementation Version: $implementationVersion and Specification Version: $specificationVersion")

  private val rollback: Boolean = args.headOption.exists(arg => if (arg == "rollback") true else false)

  // we create an instance rather than using inheritance so that this
  // class is created before the context
  private class Ctxt() extends MigrationAppContext

  implicit val ctxt: MigrationAppContext = {
    Try {
      new Ctxt()
    } match {
      case Success(context) =>
        context
      case Failure(exception) =>
        ExceptionLogger.log(Some("Failed to initialize ml-repository delete App context"), exception, None, callStack = true)
        throw utils.ServiceException(
          StatusCodes.InternalServerError,
          MLRepositoryMigrationAppFailed()
        )
    }
  }


  def deleteAsset(identity: Identity, assetId: String, container: Container): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher

    (for {
      _ <- ctxt.camsClient.deleteAsset(identity = identity, assetId = assetId, container = container, purgeOnDelete = DEFAULT_PURGE_ON_DELETE, None)
    } yield {}) recoverWith {
      case t: Throwable =>
        val msg = s"Delete asset $assetId failed for container $container due to ${t.getMessage}"
        logger.warn(msg)
        Future.successful(())
    }
  }

  def deleteAssetsJob(identity: Identity, doc: MigrationDoc): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher

    val container = if (rollback) getContainer(doc.newSpaceId, doc.newProjectId) else getContainer(doc.oldSpaceId, doc.oldProjectId)
    val assetIds = doc.results.map(_.successful).getOrElse(Seq.empty).map(result =>
      if (rollback)
        result.newId
      else
        result.oldId
    ).toVector

    seqFutures(assetIds) { assetId =>
      logger.info(s"Starting delete cams asset $assetId for container $container")
      deleteAsset(identity, assetId, container)
    }.map(_ => logger.info("finished the asset clean up for container $container"))
  }

  def deleteAssets(identity: Identity, docs: Vector[MigrationDoc]): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher

    seqFutures(docs) { doc =>
      logger.info(s"Starting delete cams assets for migration job ${doc.id.get}")
      deleteAssetsJob(identity, doc)
    }.map(_ => logger.info("finished all the delete cams assets sub job"))
  }


  def start(): Future[Unit] = {
    implicit val system: ActorSystem = ctxt.downstreamActorSystem
    implicit val ec: ExecutionContext = system.dispatcher
    sys.addShutdownHook(system.terminate())
    for {
      identity <- ctxt.getIdentity
      docs <- ctxt.dbMethods.getAllMigrationDocsByStatus(Completed)
      _ <- deleteAssets(identity, docs)
    } yield {
    }
  }
}

object MLRepositoryDeleteJobApp extends App with StrictLogging {

  Try {
    Await.result(MLRepositoryDeleteApp(args).start(), 24.hours)
  } match {
    case Success(_) =>
      logger.info("Job terminated normally")
      System.exit(0)
    case Failure(exception) =>
      ExceptionLogger.log(Some("Job terminated with an exception"), exception, None, callStack = true)
      System.exit(1)
  }
}
