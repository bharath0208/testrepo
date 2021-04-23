/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.route

import akka.actor.ActorSystem
import com.ibm.analytics.wml.api._
import com.ibm.analytics.wml.api.v4.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4.experiments.ExperimentResource
import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentEntityRequest
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.converters.ExperimentConverter
import com.ibm.ml.repository.v4.migration.job.MigrationJob
import com.ibm.ml.repository.v4.migration.models.{MigrationDoc, MigrationIds, MigrationResults}
import spray.json._

import scala.concurrent.Future

case class ExperimentMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[ExperimentResource, ExperimentEntityRequest](mj)(system,
    identity,
    v4.experiments.ExperimentJsonFormat.experimentResourceFormat,
    v4ga.experiments.ExperimentJsonFormat.experimentEntityRequestFormat
  ) with ExperimentConverter {

  override def oldAssetType: String = EXPERIMENTS_TYPE

  override def newAssetType: String = EXPERIMENTS_TYPE

  override protected def getOldAssetAction(id: String): Future[(ExperimentResource, Option[String], Seq[String])] = {
    for {
      resource <- mj.sc.v4BetaRepositoryClient.experiments.get(identity, id)
    } yield {
      (resource, None, Seq.empty)
    }

  }

  override protected def getDependenciesAction(oldResource: ExperimentResource, doc: MigrationDoc): Future[MigrationIds] = {

    val (pipelineIds, trainingLibIds, runtimeIds) = oldResource.entity.trainingReferences.map { trainingReferences =>

      val pipelineIds = trainingReferences.flatMap { tr =>
        tr.pipeline.map(_.getId())
      }
      val trainingLibIds = trainingReferences.flatMap { tr =>
        tr.trainingLib.map(_.getId())
      }
      val runtimeIds = trainingReferences.flatMap { tr =>
        tr.trainingLib.flatMap(_.runtime).map(_.href).map(hrefToId)
      }
      (pipelineIds, trainingLibIds, runtimeIds)
    } match {
      case Some(value) => value
      case None => (Seq.empty, Seq.empty, Seq.empty)
    }

    Future.successful(MigrationIds(pipelineIds = pipelineIds, trainingLibraryIds = trainingLibIds, runtimeIds = runtimeIds))
  }

  override protected def convertToRequestAction(oldResource: ExperimentResource, dependencies: MigrationIds, doc: MigrationDoc): Future[(ExperimentEntityRequest, Seq[String])] = {
    val entity = oldResource.entity
    val metadata = oldResource.metadata

    val oldEntity = oldResource.entity.toJson.asJsObject.fields - FIELD_TAGS - FIELD_SPACE - FIELD_NAME -
      FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_TRAINING_REFERENCES

    val newName = getNewName(entity.name, oldAssetType, metadata.id)
    val newIds = addSpaceOrProjectId(doc.spaceId, doc.projectId)
    val newTags = convertTags(entity.tags)
    val (newTrainingReferences, consoleMsgs) = convertTrainingReferences(entity.trainingReferences,
      mj.hardwareSpecs,
      doc.results.getOrElse(MigrationResults()),
      doc.mapping,
      metadata.id)

    import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentJsonFormat._
    val newRequest = (oldEntity ++ newName ++ newIds ++ newTags ++ newTrainingReferences).toJson.convertTo[ExperimentEntityRequest]

    Future.successful(newRequest, consoleMsgs)
  }

  override protected def createNewAssetAction(request: ExperimentEntityRequest, oldResource: ExperimentResource, doc: MigrationDoc): Future[String] = {
    for {
      (resource, _) <- mj.sc.mlRepositoryClient.experiments.create(identity, ML_REPO_VERSION, request.copy(custom = addMigratedFrom(request.custom, oldResource.metadata.id)))
    } yield {
      resource.metadata.id
    }
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.experiments.get(
      identity = identity,
      version = ML_REPO_VERSION,
      id = newId,
      spaceId = doc.spaceId,
      projectId = doc.projectId)
  }
}
