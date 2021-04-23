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
import akka.http.scaladsl.server.Route
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetMetadata}
import com.ibm.analytics.wml.api.v4ga.common.{HyperReference, Metadata, Warning}
import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4ga.experiments._
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResource
import com.ibm.ml.repository.v4.service.resources.validator.ExperimentsValidator
import com.ibm.ml.repository.v4.utils.{CallContext, logPrint}
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class ExperimentsResource(sc: ServiceContext)
                              (implicit system: ActorSystem)
  extends AbstractResource[ExperimentEntityRequest,
    ExperimentResource, ExperimentResources](sc) with ExperimentsValidator {

  override val name: String = WML_EXPERIMENT_ASSET_NAME
  override lazy val assetType: String = WML_EXPERIMENT_ASSET_TYPE
  // the endpoints defined in this class - note no support for 'content'
  override val endpoints: Route = {
    getAllExperiments ~
      createExperiment ~
      getExperiment ~
      patchExperiment ~
      deleteExperiment ~
      getExperimentRevisions ~
      createExperimentRevision
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): ExperimentResources = {

    val convertedResult: Vector[(Option[ExperimentResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    ExperimentResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): ExperimentResource = {
    val entity = entityJs.convertTo[ExperimentEntity]
    ExperimentResource(metadata, entity)
  }

  override def addWarningToResource(resource: ExperimentResource): ExperimentResource = {
    val entity = resource.entity
    val softwareSpecList = entity.trainingReferences.map(_.flatMap(_.modelDefinition).flatMap(_.softwareSpec).toVector).getOrElse(Vector.empty)
    val swSpecMessage = getSoftwareSpecWarningMessages(softwareSpecList, assetType)
    resource.copy(system = addWarningMessages(swSpecMessage, resource.system))
  }

  override def convertEntityRequestToJson(entityRequest: ExperimentEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: ExperimentEntityRequest): JsValue = {
    val jsFields = entity.toJson.asJsObject.fields
    val dependenciesMap = entity.trainingReferences.map(trs => {
      val pipeline = trs.flatMap(_.pipeline)
      //update the map with training_references[].pipeline.id
      val pipelineIds = pipeline.map(_.id).distinct
      logger.debug(s"$PIPELINE_DEPENDENCIES: $pipelineIds")
      val pipelineIdsOpt = getListOpt(pipelineIds, PIPELINE_DEPENDENCIES)
      //  * training_references[].pipeline.hardware_spec.id
      val hwSpecIds = pipeline.flatMap(_.hardwareSpec).flatMap(_.id)
      //  * training_references[].pipeline.hybrid_pipeline_hardware_specs[].hardware_spec.id
      val hwSpecIdsFromHybrid = pipeline.flatMap(_.hybridPipelineHardwareSpecs).flatten.flatMap(_.hardwareSpec.id)
      // * training_references[].model_definition.hardware_spec.id
      val hwSpecIdsFromModelDef = trs.flatMap(_.modelDefinition).flatMap(_.hardwareSpec).flatMap(_.id)

      val hwSpecIdsOpt = getListOpt((hwSpecIds ++ hwSpecIdsFromHybrid ++ hwSpecIdsFromModelDef).distinct, HARDWARE_SPEC_DEPENDENCIES)
      logger.debug(s"$HARDWARE_SPEC_DEPENDENCIES:  $hwSpecIdsOpt")

      //update the map with training_references[].model_definition.id
      val modelDefIds = trs.flatMap(_.modelDefinition).map(_.id).distinct
      logger.debug(s"$MODEL_DEFINITION_DEPENDENCIES: $modelDefIds")
      val modelDefIdsOpt = getListOpt(modelDefIds, MODEL_DEFINITION_DEPENDENCIES)

      // * training_references[].model_definition.software_spec.id
      val swSpecIds = trs.flatMap(_.modelDefinition).flatMap(_.softwareSpec).flatMap(_.id).distinct
      logger.debug(s"$SOFTWARE_SPEC_DEPENDENCIES: $swSpecIds")
      val swSpecIdsOpt = getListOpt(swSpecIds, SOFTWARE_SPEC_DEPENDENCIES)

      val dependencies = List(pipelineIdsOpt, hwSpecIdsOpt, modelDefIdsOpt, swSpecIdsOpt).flatten.reduceOption(_ ++ _).getOrElse(Map())

      logger.debug(s"Dependencies field will add to payload $dependencies")
      dependencies
    })
    import DefaultJsonProtocol._
    dependenciesMap.map(_ ++ jsFields).getOrElse(jsFields).toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: ExperimentEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }


  override def validateEntity(entity: ExperimentEntityRequest)
                             (implicit callContext: CallContext): Future[ExperimentEntityRequest] = {
    logger.trace(s"Experiment entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    val trainingReferences = entityTrainingReferencesValidator(entity.trainingReferences, sc.environmentsClient)
    for {
      trainingReferencesCopy <- trainingReferences
    } yield {
      val entityCopy = entity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId, projectId = trimProjectId,
        trainingReferences = trainingReferencesCopy)
      logger.trace(s"Experiment entity after validation ${logPrint(entityCopy.toJson)}")
      entityCopy
    }
  }

  override def getMLVersion: String = WML_EXPERIMENT_ASSET_TYPE_ML_VERSION

  private def getAllExperiments: Route = resourceAPI.getAllResources

  private def createExperiment: Route = resourceAPI.createResource

  private def getExperiment: Route = resourceAPI.getResource

  private def patchExperiment: Route = resourceAPI.patchResource

  private def deleteExperiment: Route = resourceAPI.deleteResource

  private def getExperimentRevisions: Route = resourceAPI.getAllRevisions

  private def createExperimentRevision: Route = resourceAPI.createRevision
}
