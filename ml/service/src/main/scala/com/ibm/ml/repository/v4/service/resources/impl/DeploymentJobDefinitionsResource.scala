/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources.impl

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetMetadata}
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions._
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResource
import com.ibm.ml.repository.v4.service.resources.validator.DeploymentJobDefinitionsValidator
import com.ibm.ml.repository.v4.utils.{CallContext, InvalidRequestEntityMessage, ServiceException, logPrint}
import spray.json._

import scala.collection.immutable.Map
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class DeploymentJobDefinitionsResource(sc: ServiceContext)
                                           (implicit system: ActorSystem)
  extends AbstractResource[DeploymentJobDefinitionEntityRequest,
    DeploymentJobDefinitionResource, DeploymentJobDefinitionResources](sc) with DeploymentJobDefinitionsValidator {

  override val name: String = WML_DEPLOYMENT_JOB_DEFINITION_ASSET_NAME
  override lazy val assetType: String = WML_DEPLOYMENT_JOB_DEFINITION_ASSET_TYPE
  override val spaceOnly: Boolean = true
  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllDeploymentJobDefinitions ~
      createDeploymentJobDefinition ~
      getDeploymentJobDefinition ~
      patchDeploymentJobDefinition ~
      deleteDeploymentJobDefinition ~
      getDeploymentJobDefinitionRevisions ~
      createDeploymentJobDefinitionRevision
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): DeploymentJobDefinitionResources = {

    val convertedResult: Vector[(Option[DeploymentJobDefinitionResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    DeploymentJobDefinitionResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): DeploymentJobDefinitionResource = {
    val entity = entityJs.convertTo[DeploymentJobDefinitionEntity]
    DeploymentJobDefinitionResource(metadata, entity)
  }

  override def convertEntityRequestToJson(entityRequest: DeploymentJobDefinitionEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: DeploymentJobDefinitionEntityRequest): JsValue = {
    val jsFields = entity.toJson.asJsObject.fields

    // hardware_spec.id
    val hwSpecId = entity.hardwareSpec.flatMap(_.id)
    //  hybrid_pipeline_hardware_specs[].hardware_spec.id
    val hwSpecIdsFromHybrids: Vector[Option[String]] = entity.hybridPipelineHardwareSpecs.map(_.map(_.hardwareSpec.id)).getOrElse(Vector())

    val hwSpecIdsMap = getListOpt((hwSpecIdsFromHybrids :+ hwSpecId).flatten.distinct, HARDWARE_SPEC_DEPENDENCIES).getOrElse(Map())
    logger.debug(s"$HARDWARE_SPEC_DEPENDENCIES:  $hwSpecIdsMap")

    // scoring.input_data_references[].location.id when type is data_asset
    val dataAssetIdsFromScoring: Vector[String] = entity.scoring.flatMap(_.inputDataReferences).map(inputRefs =>
      getDataAssetsIdsFromDataRefs(inputRefs)
    ).getOrElse(Vector())
    // decision_optimization.input_data_references[].location.id when type is data_asset
    val dataAssetIdsFromDecisionOptimization: Vector[String] = entity.scoring.flatMap(_.inputDataReferences).map(inputRefs =>
      getDataAssetsIdsFromDataRefs(inputRefs)
    ).getOrElse(Vector())

    val dataAssetDependencies = (dataAssetIdsFromScoring ++ dataAssetIdsFromDecisionOptimization).distinct
    val dataAssetDependenciesMap = getListOpt(dataAssetDependencies, DATA_ASSET_DEPENDENCIES).getOrElse(Map())
    logger.debug(s"$DATA_ASSET_DEPENDENCIES:  $dataAssetDependenciesMap")


    import DefaultJsonProtocol._
    (jsFields ++ hwSpecIdsMap ++ dataAssetDependenciesMap).toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: DeploymentJobDefinitionEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      Some(entity.spaceId),
      None)
  }

  override def validateEntity(entity: DeploymentJobDefinitionEntityRequest)
                             (implicit callContext: CallContext): Future[DeploymentJobDefinitionEntityRequest] = {
    logger.trace(s"deployment_job_definition entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val trimSpaceId = entity.spaceId.trim

    if (trimSpaceId.isEmpty) {
      val msg = "space_id cannot be empty string"
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }

    if (!(entity.scoring.isEmpty ^ entity.decisionOptimization.isEmpty)) { // ^ == XOR
      val msg = "Either scoring or decision_optimization has to be provided."
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }

    entityScoringValidator(entity.scoring)
    entityDecisionOptimizationValidator(entity.decisionOptimization)

    for {
      newEntity <- entityHWSpecValidator(entity, sc.environmentsClient)
    } yield {
      val entityCopy = newEntity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId)
      logger.trace(s"deployment_job_definition entity after validation ${logPrint(entityCopy.toJson)}")
      entityCopy
    }
  }

  override def validatePatchEntity(entity: List[PatchPayloadElement])
                                  (implicit callContext: CallContext): Future[JsValue] = {
    val validateMap: Map[String, PatchPayloadElement => PatchPayloadElement] = Map(
      "/deployment" -> {
        (patch: PatchPayloadElement) => {
          if (!patch.op.equals("replace")) {
            unsupportedPatchOperationError("deployment", patch.op)
          } else {
            import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
            val deployment = patch.value.map(_.convertTo[SimpleResourceRef])
            patch.copy(value = deployment.map(_.toJson))
          }
        }
      }
    )
    val patchEntity = entityValidatePatchPayloadByPathName(entity, Some(validateMap))
    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
    Future.successful(patchEntity.toJson)
  }

  override def getMLVersion: String = WML_DEPLOYMENT_JOB_DEFINITION_ASSET_TYPE_ML_VERSION

  private def getAllDeploymentJobDefinitions: Route = resourceAPI.getAllResources

  private def createDeploymentJobDefinition: Route = resourceAPI.createResource

  private def getDeploymentJobDefinition: Route = resourceAPI.getResource

  private def patchDeploymentJobDefinition: Route = resourceAPI.patchResource

  private def deleteDeploymentJobDefinition: Route = resourceAPI.deleteResource

  private def getDeploymentJobDefinitionRevisions: Route = resourceAPI.getAllRevisions

  private def createDeploymentJobDefinitionRevision: Route = resourceAPI.createRevision
}
