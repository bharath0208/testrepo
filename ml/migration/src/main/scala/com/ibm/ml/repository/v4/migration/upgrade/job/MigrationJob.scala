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
import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecificationResources
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecifications
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.models._
import com.ibm.ml.repository.v4.migration.upgrade.route.MigrationRouteRegistry
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.utils.getContainer
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
      doc7 <- SerializedJobs(migrationIds.modelDefinitionIds, "model definition", doc6, (oldId, migrationDoc) => {
        worker.modelDefinition.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 7 rev = ${doc7.rev}")
      doc8 <- SerializedJobs(migrationIds.deploymentJobDefinitionIds, "deployment job definition", doc7, (oldId, migrationDoc) => {
        worker.deploymentJobDefinition.migrate(oldId, migratedAssets, migrationDoc)
      })
      _ = logger.trace(s"doc 8 rev = ${doc8.rev}")
    } yield {
      doc8
    }
  }

  private def listAllCamsAssets(identity: Identity,
                                assetType: String,
                                spaceId: Option[String] = None,
                                projectId: Option[String] = None): Future[Seq[String]] = {
    val container = getContainer(spaceId, projectId)

    def getResult(bookmark: Option[String], result: Seq[String]): Future[Seq[String]] = {
      sc.camsClient.getAllAssetMetadata(identity, assetType, bookmark, Some(100), None, container, None).flatMap { assets =>

        if (assets.next.isDefined) {
          val bookmark = assets.next.get.bookmark
          val newResult = assets.results.map(_.metadata.assetId.get)
          getResult(bookmark, result ++ newResult)
        } else {
          Future.successful(result ++ assets.results.map(_.metadata.assetId.get))
        }
      }
    }

    getResult(None, Seq.empty)
  }

  private def getMigrationIds: Future[MigrationIds] = {
    val expIds = migrationDoc.experimentIds.getOrElse(Seq.empty)
    val funIds = migrationDoc.functionIds.getOrElse(Seq.empty)
    val modIds = migrationDoc.modelIds.getOrElse(Seq.empty)
    val pipIds = migrationDoc.pipelineIds.getOrElse(Seq.empty)
    val modDefIds = migrationDoc.modelDefinitionIds.getOrElse(Seq.empty)
    val deployJobIds = migrationDoc.deploymentJobDefinitionIds.getOrElse(Seq.empty)

    val experimentIdsFuture = if (expIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.experiments.listAllIds(identity,
        migrationDoc.oldSpaceId, migrationDoc.oldProjectId)
    } else {
      Future.successful(expIds)
    }
    val functionIdsFuture = if (funIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.functions.listAllIds(identity,
        migrationDoc.oldSpaceId, migrationDoc.oldProjectId)
    } else {
      Future.successful(funIds)
    }
    val modelIdsFuture = if (modIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.models.listAllIds(identity,
        migrationDoc.oldSpaceId, migrationDoc.oldProjectId)
    } else {
      Future.successful(modIds)
    }
    val pipelineIdsFuture = if (pipIds.contains(ASSET_ID_ALL)) {
      sc.v4BetaRepositoryClient.pipelines.listAllIds(identity,
        migrationDoc.oldSpaceId, migrationDoc.oldProjectId)
    } else {
      Future.successful(pipIds)
    }

    val modelDefinitionIdsFuture = if (modDefIds.contains(ASSET_ID_ALL)) {
      listAllCamsAssets(identity,
        WML_MODEL_DEFINITION,
        migrationDoc.oldSpaceId, migrationDoc.oldProjectId)
    } else {
      Future.successful(modDefIds)
    }

    val deploymentJobDefinitionIdsFuture = if (deployJobIds.contains(ASSET_ID_ALL)) {
      listAllCamsAssets(identity,
        WML_DEPLOYMENT_JOB_DEFINITION,
        migrationDoc.oldSpaceId, migrationDoc.oldProjectId)
    } else {
      Future.successful(deployJobIds)
    }

    for {
      experimentIds <- experimentIdsFuture
      functionIds <- functionIdsFuture
      modelIds <- modelIdsFuture
      pipelineIds <- pipelineIdsFuture
      modelDefinitionIds <- modelDefinitionIdsFuture
      deploymentJobDefinitionIds <- deploymentJobDefinitionIdsFuture
    } yield {
      MigrationIds(experimentIds, functionIds, modelIds, pipelineIds, modelDefinitionIds = modelDefinitionIds, deploymentJobDefinitionIds = deploymentJobDefinitionIds)
    }

  }

  private def getMigratedAssets(doc: MigrationDoc): Future[Seq[MigrationSuccessfulResult]] = {
    for {
      result <- sc.dbMethods.getAllMigrationJobs(identity, doc.newSpaceId, doc.newProjectId)
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
