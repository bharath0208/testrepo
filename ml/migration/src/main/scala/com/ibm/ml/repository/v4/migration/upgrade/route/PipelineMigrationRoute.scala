/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.route

import akka.actor.ActorSystem
import com.ibm.analytics.wml.api._
import com.ibm.analytics.wml.api.v4.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4.pipelines.PipelineResource
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineEntityRequest
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.converters.PipelineConverter
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds, MigrationResults}
import spray.json._

import scala.concurrent.Future
import scala.util.Try

case class PipelineMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[PipelineResource, PipelineEntityRequest](mj)(system,
    identity,
    v4.pipelines.PipelineJsonFormat.pipelineResourceFormat,
    v4ga.pipelines.PipelineJsonFormat.pipelineEntityRequestFormat
  ) with PipelineConverter {

  override def oldAssetType: String = PIPELINES_TYPE

  override def newAssetType: String = PIPELINES_TYPE

  override protected def getOldAssetAction(id: String, doc: MigrationDoc): Future[(PipelineResource, Option[String], Seq[String])] = {
    for {
      pipelineResource <- mj.sc.v4BetaRepositoryClient.pipelines.get(identity = identity, id = id, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
    } yield (pipelineResource, None, Seq.empty)
  }

  override protected def getDependenciesAction(oldResource: PipelineResource, doc: MigrationDoc): Future[MigrationIds] = {

    val (trainingLibIds, modelDefIds) = oldResource.entity.document.map { pipelineDoc => {

      val modelDefIds: Seq[String] = Try {
        pipelineDoc.pipelines.flatMap(_.nodes).flatMap(_.parameters).filter(params => {
          params.contains(FIELD_MODEL_DEFINITION)
        }).map(_ ("model_definition").asInstanceOf[JsString].value)
      }.getOrElse(Seq.empty)

      /*
      Case where training_lib_href could contain reference to model_definition
      "parameters": {
        "training_lib_href": "/v2/assets/d74b4294-4bb3-429e-bb0b-eabb0ca161ea?space_id=fd8e41ff-5a58-47b4-a561-27f328db0f85"
      }
      We assume that if training_lib_href contains a CAMS URL, it is a model_definition
      */
      val trainingLibModelDefIds = pipelineDoc.pipelines.
        flatMap(_.nodes).
        flatMap(_.parameters).
        flatMap(_.get(FIELD_TRAINING_LIB_HREF)).
        map(_.asInstanceOf[JsString].value).
        filter(_.contains("/v2/assets/")).
        map(hrefToId)

      val trainingLibIds = pipelineDoc.pipelines.
        flatMap(_.nodes).
        flatMap(_.parameters).
        flatMap(_.get(FIELD_TRAINING_LIB_HREF)).
        map(_.asInstanceOf[JsString].value).
        filter(_.contains("/v4/libraries/")).
        map(hrefToId)

      (trainingLibIds, modelDefIds ++ trainingLibModelDefIds)

    }
    }.getOrElse(Seq.empty, Seq.empty)

    Future.successful(MigrationIds(trainingLibraryIds = trainingLibIds, modelDefinitionIds = modelDefIds))
  }

  override protected def convertToRequestAction(oldResource: PipelineResource, dependencies: MigrationIds, doc: MigrationDoc): Future[(PipelineEntityRequest, Seq[String])] = {
    val entity = oldResource.entity
    val metadata = oldResource.metadata

    val oldEntity = oldResource.entity.toJson.asJsObject.fields - FIELD_TAGS - FIELD_SPACE - FIELD_NAME -
      FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_DOCUMENT

    val newName = getNewName(entity.name, oldAssetType, metadata.id)
    val newIds = addSpaceOrProjectId(doc.newSpaceId, doc.newProjectId)
    val newTags = convertTags(entity.tags)
    val (newDocument, consoleMsgs) = convertPipelineDoc(entity.document,
      mj.softwareSpecs,
      mj.hardwareSpecs,
      doc.mapping,
      doc.results.getOrElse(MigrationResults()),
      metadata.id)

    import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
    val newRequest = (oldEntity ++ newName ++ newIds ++ newTags ++ newDocument).toJson.convertTo[PipelineEntityRequest]
    Future.successful(newRequest, consoleMsgs)

  }

  override protected def createNewAssetAction(request: PipelineEntityRequest, oldResource: PipelineResource, doc: MigrationDoc): Future[String] = {
    for {
      (v4gaPipelineResource, _) <- mj.sc.mlRepositoryClient.pipelines.create(identity = identity, version = ML_REPO_VERSION, entity = request.copy(custom = addMigratedFrom(request.custom, oldResource.metadata.id, doc)))
    } yield v4gaPipelineResource.metadata.id
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.pipelines.get(
      identity = identity,
      version = ML_REPO_VERSION,
      id = newId,
      spaceId = doc.newSpaceId,
      projectId = doc.newProjectId)
  }

}
