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
import com.ibm.analytics.wml.api.v4ga.common.{HyperReference, Metadata, SoftwareSpecRef, Warning}
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4ga.pipelines._
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResource
import com.ibm.ml.repository.v4.service.resources.validator.PipelinesValidator
import com.ibm.ml.repository.v4.utils.{CallContext, logPrint}
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class PipelinesResource(sc: ServiceContext)
                            (implicit system: ActorSystem)
  extends AbstractResource[PipelineEntityRequest,
    PipelineResource, PipelineResources](sc) with PipelinesValidator {

  override val name: String = WML_PIPELINE_ASSET_NAME
  override lazy val assetType: String = WML_PIPELINE_ASSET_TYPE
  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllPipelines ~
      createPipeline ~
      getPipeline ~
      patchPipeline ~
      deletePipeline ~
      getPipelineRevisions ~
      createPipelineRevision
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): PipelineResources = {

    val convertedResult: Vector[(Option[PipelineResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    PipelineResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): PipelineResource = {
    val entity = entityJs.convertTo[PipelineEntity]
    PipelineResource(metadata, entity)
  }

  override def addWarningToResource(resource: PipelineResource): PipelineResource = {
    val entity = resource.entity

    val softwareSpecList = entity.document.map(doc => {
      val softwareSpecsIdsFromRuntimes: Seq[SoftwareSpecRef] = doc.runtimes.map(runtimes => {
        runtimes.flatMap(_.appData).flatMap(_.wmlData.softwareSpec)
      }).getOrElse(Seq.empty).map(SoftwareSpecRef(_))
      val softwareSpecsIdFromAppData: Seq[SoftwareSpecRef] = doc.appData.flatMap(_.wmlData.softwareSpec).toList.map(SoftwareSpecRef(_))
      softwareSpecsIdsFromRuntimes ++ softwareSpecsIdFromAppData
    }).getOrElse(Seq.empty).toVector

    val swSpecMessage = getSoftwareSpecWarningMessages(softwareSpecList, assetType)
    resource.copy(system = addWarningMessages(swSpecMessage, resource.system))
  }

  override def convertEntityRequestToJson(entityRequest: PipelineEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: PipelineEntityRequest): JsValue = {
    val jsFields = entity.toJson.asJsObject.fields
    // update nodes parameter and get modelDefId list
    val dependenciesMap = entity.document.map(doc => {
      // Model def id in document.pipelines[].nodes[].parameters["model_definition"]
      val modelDefIds: Seq[String] = doc.pipelines.flatMap(_.nodes).flatMap(_.parameters).filter(params => {
        params.contains("model_definition") && params("model_definition").asJsObject.fields.contains("id")
      }).map(_ ("model_definition").asJsObject.fields("id").asInstanceOf[JsString].value)
      logger.debug(s"$MODEL_DEFINITION_DEPENDENCIES: $modelDefIds")
      val modelDefIdsOpt = getListOpt(modelDefIds, MODEL_DEFINITION_DEPENDENCIES)

      // document.runtimes[].appData.wmlData.hardwareSpec.id
      val hardwareSpecsIdsFromRuntimes: Seq[String] = doc.runtimes.map(runtimes => {
        runtimes.flatMap(_.appData).flatMap(_.wmlData.hardwareSpec).flatMap(_.id)
      }).getOrElse(Seq.empty)
      // document.runtimes[].appData.wmlData.softwareSpec.ids
      val softwareSpecsIdsFromRuntimes: Seq[String] = doc.runtimes.map(runtimes => {
        runtimes.flatMap(_.appData).flatMap(_.wmlData.softwareSpec).flatMap(_.id)
      }).getOrElse(Seq.empty)
      // document.appData.wmlData.hardwareSpec.id
      val hardwareSpecsIdFromAppData: Seq[String] = doc.appData.flatMap(_.wmlData.hardwareSpec).flatMap(_.id).toList
      //  document.appData.wmlData.softwareSpec.id
      val softwareSpecsIdFromAppData: Seq[String] = doc.appData.flatMap(_.wmlData.softwareSpec).flatMap(_.id).toList

      val hwSpecIdsOpt = getListOpt((hardwareSpecsIdsFromRuntimes ++ hardwareSpecsIdFromAppData).distinct, HARDWARE_SPEC_DEPENDENCIES)
      logger.debug(s"$HARDWARE_SPEC_DEPENDENCIES:  $hwSpecIdsOpt")

      val swSpecIdsOpt = getListOpt((softwareSpecsIdsFromRuntimes ++ softwareSpecsIdFromAppData).distinct, SOFTWARE_SPEC_DEPENDENCIES)
      logger.debug(s"$SOFTWARE_SPEC_DEPENDENCIES:  $swSpecIdsOpt")

      val dependencies = List(modelDefIdsOpt ++ swSpecIdsOpt ++ hwSpecIdsOpt).flatten.reduceOption(_ ++ _).getOrElse(Map())
      dependencies
    })
    import DefaultJsonProtocol._
    dependenciesMap.map(_ ++ jsFields).getOrElse(jsFields).toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: PipelineEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }

  override def validateEntity(entity: PipelineEntityRequest)
                             (implicit callContext: CallContext): Future[PipelineEntityRequest] = {
    logger.trace(s"Pipeline entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    val document = entityPipelineDocumentValidator(entity.document, sc.environmentsClient)
    for {
      documentCopy <- document
    } yield {
      val entityCopy = entity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId,
        projectId = trimProjectId, document = documentCopy)
      logger.trace(s"Pipeline entity after validation ${logPrint(entityCopy.toJson)}")
      entityCopy
    }
  }

  override def getMLVersion: String = WML_PIPELINE_ASSET_TYPE_ML_VERSION

  private def getAllPipelines: Route = resourceAPI.getAllResources

  private def createPipeline: Route = resourceAPI.createResource

  private def getPipeline: Route = resourceAPI.getResource

  private def patchPipeline: Route = resourceAPI.patchResource

  private def deletePipeline: Route = resourceAPI.deleteResource

  private def getPipelineRevisions: Route = resourceAPI.getAllRevisions

  private def createPipelineRevision: Route = resourceAPI.createRevision

}
