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
import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4.common.Tag
import com.ibm.analytics.wml.api.v4.jobs.JobScoringResult
import com.ibm.analytics.wml.api.v4.jobs.JobsJsonFormat._
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionEntityRequest
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.converters.ConversionHelper
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds}
import com.ibm.ml.repository.v4.utils.getContainer
import spray.json._

import scala.concurrent.Future

case class DeploymentJobDefinitionMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[Asset, DeploymentJobDefinitionEntityRequest](mj)(
    system,
    identity,
    com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat.assetFormat,
    v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat.deploymentJobDefinitionEntityRequestFormat
  ) with ConversionHelper {

  override def oldAssetType: String = DEPLOYMENT_JOB_DEFINITIONS_TYPE

  override def newAssetType: String = DEPLOYMENT_JOB_DEFINITIONS_TYPE

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
  Future[(DeploymentJobDefinitionEntityRequest, Seq[String])] = {
    val entityMap = oldResource.entity.get.fields(WML_DEPLOYMENT_JOB_DEFINITION).asJsObject.fields

    val oldEntity = entityMap -
      FIELD_SPACE - FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_TAGS - FIELD_SCORING

    val newIds = addSpaceOrProjectId(doc.newSpaceId, doc.newProjectId)
    import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat._
    val newTags = convertSeqTags(entityMap.get(FIELD_TAGS).map(_.convertTo[Seq[Tag]]))
    val newName = jsonMap(FIELD_NAME, oldResource.metadata.name.toJson)
    val newScoring = entityMap.get(FIELD_SCORING).map(_.convertTo[JobScoringResult]).map { scoring =>
      // remove environmentVariables if it is not a JsObject
      val envVars = scoring.environmentVariables.flatMap{ vs => if(vs.isInstanceOf[JsObject]) Some(vs) else None }
      jsonMap(FIELD_SCORING, scoring.copy(environmentVariables = envVars).toJson)
    }.getOrElse(Map.empty)

    val newJson = (oldEntity ++ newName ++ newIds ++ newTags ++ newScoring).toJson
    val newRequest = newJson.convertTo[DeploymentJobDefinitionEntityRequest]
    Future.successful(newRequest, Seq.empty)
  }

  override protected def createNewAssetAction(request: DeploymentJobDefinitionEntityRequest, oldResource: Asset, doc: MigrationDoc): Future[String] = {
    val oldId = oldResource.metadata.assetId.getOrElse("")
    for {
      (resource, _) <- mj.sc.mlRepositoryClient.deployment_job_definitions.create(identity, ML_REPO_VERSION, request.copy(custom = addMigratedFrom(request.custom, oldId, doc)))
    } yield {
      resource.metadata.id
    }
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.deployment_job_definitions.get(
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
