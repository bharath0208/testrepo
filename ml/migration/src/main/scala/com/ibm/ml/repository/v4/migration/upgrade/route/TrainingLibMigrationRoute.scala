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
import com.ibm.analytics.wml.api.v4.libraries.LibraryJsonFormat._
import com.ibm.analytics.wml.api.v4.libraries.LibraryResource
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionEntityRequest
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.converters.ConversionHelper
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds}
import spray.json._

import scala.concurrent.Future

case class TrainingLibMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[LibraryResource, ModelDefinitionEntityRequest](mj) with ConversionHelper {

  override def oldAssetType: String = TRAINING_LIBS_TYPE

  override def newAssetType: String = MODEL_DEFINITIONS_TYPE

  override protected def getOldAssetAction(id: String, doc: MigrationDoc): Future[(LibraryResource, Option[String], Seq[String])] = {
    for {
      resource <- mj.sc.v4BetaRepositoryClient.library.get(identity, id, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
    } yield {
      (resource, None, Seq.empty)
    }
  }

  override protected def getDependenciesAction(oldResource: LibraryResource,
                                               doc: MigrationDoc): Future[MigrationIds] =
    Future.successful(MigrationIds())

  override protected def convertToRequestAction(oldResource: LibraryResource,
                                                dependencies: MigrationIds,
                                                doc: MigrationDoc):
  Future[(ModelDefinitionEntityRequest, Seq[String])] = {
    val entity = oldResource.entity

    val oldEntity = oldResource.entity.toJson.asJsObject.fields - FIELD_MODEL_DEFINITION -
      FIELD_SPACE - FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_TAGS

    val newIds = addSpaceOrProjectId(doc.newSpaceId, doc.newProjectId)
    val newTags = convertSeqTags(entity.tags)

    import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
    val newRequest = (oldEntity ++ newIds ++ newTags).toJson.convertTo[ModelDefinitionEntityRequest]
    Future.successful(newRequest, Seq.empty)
  }

  override protected def createNewAssetAction(request: ModelDefinitionEntityRequest, oldResource: LibraryResource, doc: MigrationDoc): Future[String] = {
    val oldId = oldResource.metadata.id
    val projectId = doc.newProjectId
    val spaceId = doc.newSpaceId

    for {
      (resource, _) <- mj.sc.mlRepositoryClient.model_definitions.create(identity, ML_REPO_VERSION, request.copy(custom = addMigratedFrom(request.custom, oldResource.metadata.id, doc)))

      _ <- {
        (for {
          source <- mj.sc.v4BetaRepositoryClient.library.download(identity, oldId, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
          _ <- mj.sc.mlRepositoryClient.model_definitions.upload(identity,
            ML_REPO_VERSION,
            resource.metadata.id,
            content = source,
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
}
