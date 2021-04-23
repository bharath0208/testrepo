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

import java.util.Date

import akka.actor.ActorSystem
import com.ibm.analytics.cams.api.v2.assets.Asset
import com.ibm.analytics.wml.api._
import com.ibm.analytics.wml.api.v4.common.Tag
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionEntityRequest
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.cams.DataAssetType
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.converters.ConversionHelper
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds}
import com.ibm.ml.repository.v4.utils.getContainer
import spray.json._

import scala.concurrent.Future

case class ModelDefinitionMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[Asset, ModelDefinitionEntityRequest](mj)(
    system,
    identity,
    com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat.assetFormat,
    v4ga.model_definitions.ModelDefinitionJsonFormat.modelDefinitionEntityRequestFormat
  ) with ConversionHelper {

  override def oldAssetType: String = MODEL_DEFINITIONS_TYPE

  override def newAssetType: String = MODEL_DEFINITIONS_TYPE

  override protected def getOldAssetAction(id: String, doc: MigrationDoc): Future[(Asset, Option[String], Seq[String])] = {
    for {
      oldModelDef <- mj.sc.camsClient.getAssetMetadata(identity, id, None, getContainer(doc.oldSpaceId, doc.oldProjectId), None)
    } yield (oldModelDef, None, Seq.empty)
  }

  override protected def getDependenciesAction(oldResource: Asset, doc: MigrationDoc): Future[MigrationIds] = {
    Future.successful(MigrationIds())
  }

  override protected def convertToRequestAction(oldResource: Asset,
                                                dependencies: MigrationIds,
                                                doc: MigrationDoc):
  Future[(ModelDefinitionEntityRequest, Seq[String])] = {
    val entityMap = oldResource.entity.get.fields(WML_MODEL_DEFINITION).asJsObject.fields

    val oldEntity = entityMap - FIELD_MODEL_DEFINITION -
      FIELD_SPACE - FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_TAGS

    val newIds = addSpaceOrProjectId(doc.newSpaceId, doc.newProjectId)
    import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
    val newTags = convertSeqTags(entityMap.get(FIELD_TAGS).map(_.convertTo[Seq[Tag]]))
    val newName = jsonMap(FIELD_NAME, oldResource.metadata.name.toJson)

    val newRequest = (oldEntity ++ newIds ++ newTags ++ newName).toJson.convertTo[ModelDefinitionEntityRequest]
    Future.successful(newRequest, Seq.empty)
  }

  override protected def createNewAssetAction(request: ModelDefinitionEntityRequest, oldResource: Asset, doc: MigrationDoc): Future[String] = {
    val oldId = oldResource.metadata.assetId.getOrElse("")
    val projectId = doc.newProjectId
    val spaceId = doc.newSpaceId
    for {
      (resource, _) <- mj.sc.mlRepositoryClient.model_definitions.create(identity, ML_REPO_VERSION, request.copy(custom = addMigratedFrom(request.custom, oldId, doc)))
      _ <- {
        (for {
          assetAttachmentMeta <- mj.sc.camsClient.getAssetAttachmentMetadata(identity, oldResource.attachments.get(0).attachmentId, oldId, None, getContainer(doc.oldSpaceId, doc.oldProjectId), None)
          downloadResponse <- mj.sc.camsClient.downloadAttachmentUsingSignedUrl(identity, assetAttachmentMeta, DataAssetType.managed)
          _ <- mj.sc.mlRepositoryClient.model_definitions.upload(identity,
            ML_REPO_VERSION,
            resource.metadata.id,
            content = downloadResponse.entity.dataBytes,
            contentType = downloadResponse.entity.contentType,
            projectId = projectId,
            spaceId = spaceId
          )
        } yield {

        }) recoverWith {
          case e: Throwable =>
            // if the model_definition does not contains content, we will ignore the error
            val msg = s"Skip to migrate the content for the model_definition asset $oldId: ${e.getMessage}"
            logger.info(msg)
            Future.successful(())
        }
      }
    } yield {
      resource.metadata.id
    }
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.model_definitions.get(
      identity = identity,
      version = ML_REPO_VERSION,
      id = newId,
      spaceId = doc.newSpaceId,
      projectId = doc.newProjectId)
  }

  override protected def getNameAndLastModified(oldResource: Option[Asset]): (Option[String], Option[Date]) = {
    val name = oldResource.map(_.metadata.name)
    val lastModified = oldResource.flatMap(_.metadata.usage).flatMap(_.lastUpdateTime).map(new Date(_))
    (name, lastModified)
  }
}
