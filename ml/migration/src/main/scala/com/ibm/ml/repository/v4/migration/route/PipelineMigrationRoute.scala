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
import com.ibm.analytics.wml.api.v4.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4.pipelines.PipelineResource
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineEntityRequest
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.converters.PipelineConverter
import com.ibm.ml.repository.v4.migration.job.MigrationJob
import com.ibm.ml.repository.v4.migration.models.{MigrationDoc, MigrationIds, MigrationResults}
import spray.json._

import scala.concurrent.Future

case class PipelineMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[PipelineResource, PipelineEntityRequest](mj)(system,
    identity,
    v4.pipelines.PipelineJsonFormat.pipelineResourceFormat,
    v4ga.pipelines.PipelineJsonFormat.pipelineEntityRequestFormat
  ) with PipelineConverter {

  override def oldAssetType: String = PIPELINES_TYPE

  override def newAssetType: String = PIPELINES_TYPE

  override protected def getOldAssetAction(id: String): Future[(PipelineResource, Option[String], Seq[String])] = {
    for {
      pipelineResource <- mj.sc.v4BetaRepositoryClient.pipelines.get(identity = identity, id = id)
    } yield (pipelineResource, None, Seq.empty)
  }

  override protected def getDependenciesAction(oldResource: PipelineResource, doc: MigrationDoc): Future[MigrationIds] = {

    val trainingLibId = oldResource.entity.document.map { pipelineDoc => {
      pipelineDoc.pipelines.
        flatMap(_.nodes).
        flatMap(_.parameters).
        flatMap(_.get(FIELD_TRAINING_LIB_HREF)).
        map(_.asInstanceOf[JsString].value.split("/").last)
    }
    }.getOrElse(Seq.empty)

    Future.successful(MigrationIds(trainingLibraryIds = trainingLibId))
  }

  override protected def convertToRequestAction(oldResource: PipelineResource, dependencies: MigrationIds, doc: MigrationDoc): Future[(PipelineEntityRequest, Seq[String])] = {
    val entity = oldResource.entity
    val metadata = oldResource.metadata

    val oldEntity = oldResource.entity.toJson.asJsObject.fields - FIELD_TAGS - FIELD_SPACE - FIELD_NAME -
      FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_DOCUMENT

    val newName = getNewName(entity.name, oldAssetType, metadata.id)
    val newIds = addSpaceOrProjectId(doc.spaceId, doc.projectId)
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
      (v4gaPipelineResource, _) <- mj.sc.mlRepositoryClient.pipelines.create(identity = identity, version = ML_REPO_VERSION, entity = request.copy(custom = addMigratedFrom(request.custom, oldResource.metadata.id)))
    } yield v4gaPipelineResource.metadata.id
  }


  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.pipelines.get(
      identity = identity,
      version = ML_REPO_VERSION,
      id = newId,
      spaceId = doc.spaceId,
      projectId = doc.projectId)
  }

}
