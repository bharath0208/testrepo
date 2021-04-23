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
import com.ibm.analytics.wml.api.v4ga.common.{HyperReference, Metadata, PatchPayloadElement, Warning}
import com.ibm.analytics.wml.api.v4ga.training_definitions.TrainingDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.training_definitions._
import com.ibm.analytics.wml.api.v4ga.trainings.fl.FLJsonFormat.federatedLearningFormat
import com.ibm.analytics.wml.api.v4ga.trainings.fl.FederatedLearning
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResource
import com.ibm.ml.repository.v4.service.resources.validator.TrainingDefinitionsValidator
import com.ibm.ml.repository.v4.utils.{CallContext, InvalidRequestEntityMessage, ServiceException, logPrint}
import spray.json._

import scala.collection.immutable.Map
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class TrainingDefinitionsResource(sc: ServiceContext)
                                      (implicit system: ActorSystem)
  extends AbstractResource[TrainingDefinitionEntityRequest,
    TrainingDefinitionResource, TrainingDefinitionResources](sc) with TrainingDefinitionsValidator {

  override val name: String = WML_TRAINING_DEFINITION_ASSET_NAME
  override lazy val assetType: String = WML_TRAINING_DEFINITION_ASSET_TYPE
  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllTrainingDefinitions ~
      createTrainingDefinition ~
      getTrainingDefinition ~
      patchTrainingDefinition ~
      deleteTrainingDefinition ~
      getTrainingDefinitionRevisions ~
      createTrainingDefinitionRevision
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): TrainingDefinitionResources = {

    val convertedResult: Vector[(Option[TrainingDefinitionResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    TrainingDefinitionResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): TrainingDefinitionResource = {
    val entity = entityJs.convertTo[TrainingDefinitionEntity]
    TrainingDefinitionResource(metadata, entity)
  }

  override def addWarningToResource(resource: TrainingDefinitionResource): TrainingDefinitionResource = {
    val entity = resource.entity
    val softwareSpecList = entity.modelDefinition.flatMap(_.softwareSpec.map(Vector(_))).getOrElse(Vector.empty)
    val swSpecMessage = getSoftwareSpecWarningMessages(softwareSpecList, assetType)
    resource.copy(system = addWarningMessages(swSpecMessage, resource.system))
  }

  override def convertEntityRequestToJson(entityRequest: TrainingDefinitionEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: TrainingDefinitionEntityRequest): JsValue = {
    val jsFields = entity.toJson.asJsObject.fields

    // pipeline.hardware_spec.id
    val hwSpecIdFromPipeline = entity.pipeline.flatMap(_.hardwareSpec).flatMap(_.id)
    //  pipeline.hybrid_pipeline_hardware_specs[].hardware_spec.id
    val hwSpecIdsFromHybrids: Vector[Option[String]] = entity.pipeline.flatMap(_.hybridPipelineHardwareSpecs)
      .map(_.map(_.hardwareSpec.id)).getOrElse(Vector())
    // model_definition.hardware_spec.id
    val hwSpecIdsFromModelDef = entity.modelDefinition.flatMap(_.hardwareSpec).flatMap(_.id)

    val hwSpecIdsMap = getListOpt((hwSpecIdsFromHybrids :+ hwSpecIdFromPipeline :+ hwSpecIdsFromModelDef).flatten.distinct, HARDWARE_SPEC_DEPENDENCIES).getOrElse(Map())
    logger.debug(s"$HARDWARE_SPEC_DEPENDENCIES:  $hwSpecIdsMap")


    // training_data_references[].location.id when type is data_asset
    val dataAssetDependencies: Vector[String] = getDataAssetsIdsFromDataRefs(entity.trainingDataReferences).distinct
    val dataAssetDependenciesMap = getListOpt(dataAssetDependencies, DATA_ASSET_DEPENDENCIES).getOrElse(Map())
    logger.debug(s"$DATA_ASSET_DEPENDENCIES:  $dataAssetDependenciesMap")


    val experiment = entity.experiment
    val pipeline = entity.pipeline
    val modelDef = entity.modelDefinition
    val fl = entity.federatedLearning
    val definitionType = (experiment, pipeline, modelDef, fl) match {
      case (Some(_), None, None, None) => EXPERIMENT_TYPE
      case (None, Some(_), None, None) => PIPELINE_TYPE
      case (None, None, Some(_), None) => MODEL_DEFINITION_TYPE
      case (None, None, None, Some(_)) => FEDERATED_LEARNING_TYPE
      case _ =>
        val msg = "Either 'experiment', 'pipeline', 'model_definition' or 'federated_learning' field needs to be provided"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
    import DefaultJsonProtocol._
    val typeMap = Map(DEFINITION_TYPE -> definitionType.toJson)
    (jsFields ++ hwSpecIdsMap ++ dataAssetDependenciesMap ++ typeMap).toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: TrainingDefinitionEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }

  override def validateEntity(entity: TrainingDefinitionEntityRequest)
                             (implicit callContext: CallContext): Future[TrainingDefinitionEntityRequest] = {
    logger.debug(s"training_definition entity before validation ${logPrint(entity.toJson)}")

    trainingFieldValidator(entity)
    val trimName = unsafeCharactersValidator(entity.name, "name", true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    val trainingDataReferences = entityTrainingDataReferencesValidator(entity.trainingDataReferences)
    val resultsReference = ObjectLocationValidator(entity.resultsReference)

    for {
      newEntity <- entityHWSWSpecValidator(entity, sc.environmentsClient)
    } yield {
      val entityCopy = newEntity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId,
        projectId = trimProjectId, trainingDataReferences = trainingDataReferences, resultsReference = resultsReference)
      logger.trace(s"training_definition entity after validation ${logPrint(entityCopy.toJson)}")
      entityCopy
    }
  }

  override def validatePatchEntity(entity: List[PatchPayloadElement])
                                  (implicit callContext: CallContext): Future[JsValue] = {
    val validateMap: Map[String, PatchPayloadElement => PatchPayloadElement] = Map(
      "/federated_learning" -> {
        (patch: PatchPayloadElement) => {
          if (patch.op.equals("replace")) {
            val fl = patch.value.map(_.convertTo[FederatedLearning])
            patch.copy(value = fl.map(_.toJson))
          } else {
            val msg = s"Only replace is allowed for the federated_learning field"
            throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
          }
        }
      }
    )
    val patchEntity = entityValidatePatchPayloadByPathName(entity, Some(validateMap))
    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
    Future.successful(patchEntity.toJson)
  }

  override def getMLVersion: String = WML_TRAINING_DEFINITION_ASSET_TYPE_ML_VERSION

  private def getAllTrainingDefinitions: Route = resourceAPI.getAllResources

  private def createTrainingDefinition: Route = resourceAPI.createResource

  private def getTrainingDefinition: Route = resourceAPI.getResource

  private def patchTrainingDefinition: Route = resourceAPI.patchResource

  private def deleteTrainingDefinition: Route = resourceAPI.deleteResource

  private def getTrainingDefinitionRevisions: Route = resourceAPI.getAllRevisions

  private def createTrainingDefinitionRevision: Route = resourceAPI.createRevision

}
