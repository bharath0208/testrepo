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
import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecificationResources
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecifications
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.models._
import com.ibm.ml.repository.v4.migration.route.MigrationRouteRegistry
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.{ExecutionContext, Future}

case class MigrationJob(sc: MigrationAppContext,
                        migrationDoc: MigrationDoc,
                        softwareSpecs: SoftwareSpecifications,
                        hardwareSpecs: HardwareSpecificationResources)(implicit identity: Identity) extends StrictLogging with MigrationConstant {
  implicit val system: ActorSystem = sc.downstreamActorSystem
  implicit val ec: ExecutionContext = system.dispatcher
  val worker: MigrationRouteRegistry = MigrationRouteRegistry(this)

  private def SerializedJobs(ids: Seq[String],
                             assetType: String,
                             doc: MigrationDoc,
                             f: (String, MigrationDoc) => Future[MigrationDoc]): Future[MigrationDoc] = {
    var fAccum: Future[MigrationDoc] = Future {
      doc
    }
    for (id <- ids) {
      logger.info(s"Start to migrate $assetType asset $id")
      fAccum = fAccum flatMap { document => f(id, document) }
    }
    fAccum
  }

  def startMigrate(migrationIds: MigrationIds,
                   migratedAssets: Seq[MigrationSuccessfulResult],
                   doc: MigrationDoc): Future[MigrationDoc] = {

    for {
      doc0 <- SerializedJobs(migrationIds.experimentIds, "experiment", doc, (oldId, migrationDoc) => {
        worker.experiment.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 0 rev = ${doc0.rev}")
      doc1 <- SerializedJobs(migrationIds.functionIds, "function", doc0, (oldId, migrationDoc) => {
        worker.function.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 1 rev = ${doc1.rev}")
      doc2 <- SerializedJobs(migrationIds.modelIds, "model", doc1, (oldId, migrationDoc) => {
        worker.model.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 2 rev = ${doc2.rev}")
      doc3 <- SerializedJobs(migrationIds.pipelineIds, "pipeline", doc2, (oldId, migrationDoc) => {
        worker.pipeline.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 3 rev = ${doc3.rev}")
      doc4 <- SerializedJobs(migrationIds.trainingLibraryIds, "training library", doc3, (oldId, migrationDoc) => {
        worker.trainingLib.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 4 rev = ${doc4.rev}")
      doc5 <- SerializedJobs(migrationIds.runtimeIds, "runtime", doc4, (oldId, migrationDoc) => {
        worker.runtime.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 5 rev = ${doc5.rev}")
      doc6 <- SerializedJobs(migrationIds.libraryIds, "library", doc5, (oldId, migrationDoc) => {
        worker.library.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 6 rev = ${doc6.rev}")
    } yield {
      doc6
    }
  }

  private def getMigrationIds: Future[MigrationIds] = {
    val expIds = migrationDoc.experimentIds.getOrElse(Seq.empty)
    val funIds = migrationDoc.functionIds.getOrElse(Seq.empty)
    val modIds = migrationDoc.modelIds.getOrElse(Seq.empty)
    val pipIds = migrationDoc.pipelineIds.getOrElse(Seq.empty)

    val experimentIdsFuture = if (expIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.experiments.listAllIds(identity)
    } else {
      Future.successful(expIds)
    }
    val functionIdsFuture = if (funIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.functions.listAllIds(identity)
    } else {
      Future.successful(funIds)
    }
    val modelIdsFuture = if (modIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.models.listAllIds(identity)
    } else {
      Future.successful(modIds)
    }
    val PipelineIdsFuture = if (pipIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.pipelines.listAllIds(identity)
    } else {
      Future.successful(pipIds)
    }

    for {
      experimentIds <- experimentIdsFuture
      functionIds <- functionIdsFuture
      modelIds <- modelIdsFuture
      pipelineIds <- PipelineIdsFuture
    } yield {
      MigrationIds(experimentIds, functionIds, modelIds, pipelineIds)
    }

  }

  private def getMigratedAssets(doc: MigrationDoc): Future[Seq[MigrationSuccessfulResult]] = {
    for {
      result <- sc.dbMethods.getAllMigrationJobs(identity, doc.spaceId, doc.projectId)
    } yield {
      result.resources.flatMap(_.results).flatMap(_.successful).distinct
    }
  }

  // run Migration job here, need to pass the arg to the start
  def start(): Future[Unit] = {
    for {
      migrationIds <- getMigrationIds
      migratedAssets <- getMigratedAssets(migrationDoc)
      doc0 <- sc.dbMethods.updateStatus(Running, migrationDoc)
      doc1 <- startMigrate(migrationIds, migratedAssets, doc0)
      _ <- sc.dbMethods.updateStatus(Completed, doc1)
    } yield {
    }
  }
}
