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
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.model_definitions._
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResourceWithContent
import com.ibm.ml.repository.v4.service.resources.validator.ModelDefinitionsValidator
import com.ibm.ml.repository.v4.utils.{CallContext, logPrint}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class ModelDefinitionsResource(sc: ServiceContext)
                                   (implicit system: ActorSystem)
  extends AbstractResourceWithContent[ModelDefinitionEntityRequest,
    ModelDefinitionResource, ModelDefinitionResources](sc) with ModelDefinitionsValidator {

  override val name: String = WML_MODEL_DEFINITION_ASSET_NAME
  override lazy val assetType: String = WML_MODEL_DEFINITION_ASSET_TYPE
  override val contentAPIName: Option[String] = Some(WML_MODEL_DEFINITION_CONTENT_API_NAME)

  val uploadContentTypesAllowed: List[String] = List(
    "application/zip",
    "application/gzip",
    "application/octet-stream"
  )

  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllModelDefinitions ~
      createModelDefinition ~
      getModelDefinition ~
      patchModelDefinition ~
      deleteModelDefinition ~
      getModelDefinitionRevisions ~
      createModelDefinitionRevision ~
      downloadModelDefinitionContent ~
      uploadModelDefinitionContent
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): ModelDefinitionResources = {
    val convertedResult: Vector[(Option[ModelDefinitionResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    ModelDefinitionResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): ModelDefinitionResource = {
    val entity = entityJs.convertTo[ModelDefinitionEntity]
    ModelDefinitionResource(metadata, entity)
  }

  override def addWarningToResource(resource: ModelDefinitionResource): ModelDefinitionResource = {
    val entity = resource.entity
    val softwareSpecList = entity.softwareSpec.map(Vector(_)).getOrElse(Vector.empty)
    val swSpecMessage = getSoftwareSpecWarningMessages(softwareSpecList, assetType)
    resource.copy(system = addWarningMessages(swSpecMessage, resource.system))
  }

  override def convertEntityRequestToJson(entityRequest: ModelDefinitionEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: ModelDefinitionEntityRequest): JsValue = {
    entity.toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: ModelDefinitionEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }

  override def validateEntity(entity: ModelDefinitionEntityRequest)
                             (implicit callContext: CallContext): Future[ModelDefinitionEntityRequest] = {
    logger.trace(s"model_definition entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", blankCheck = true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val version = stringFieldValidator(entity.version, "version")
    val platformName = stringFieldValidator(entity.platform.name, "platform.name")
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)

    for {
      swSpecCopy <- softwareSpecValidator(entity.softwareSpec, sc.environmentsClient)
    } yield {
      val entityCopy = entity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId,
        projectId = trimProjectId, version = version, platform = Platform(platformName, entity.platform.versions), softwareSpec = swSpecCopy)
      logger.trace(s"model_definition entity after validation ${logPrint(entityCopy.toJson)}")
      entityCopy
    }
  }

  override def validateSingleUpload(resource: Asset, contentMimeType: String, camsClient: CAMSClient)(implicit callContext: CallContext, ec: ExecutionContext): Future[Unit] = {
    validateUploadContentType(contentMimeType, uploadContentTypesAllowed)
    for (
      // check if content already exist, if true we need to delete it
      _ <- deleteAllAttachmentsIfExist(resource, camsClient)
    ) yield {}
  }

  override def getMLVersion: String = WML_MODEL_DEFINITION_ASSET_TYPE_ML_VERSION

  private def getAllModelDefinitions: Route = resourceAPI.getAllResources

  private def createModelDefinition: Route = resourceAPI.createResource

  private def getModelDefinition: Route = resourceAPI.getResource

  private def patchModelDefinition: Route = resourceAPI.patchResource

  private def deleteModelDefinition: Route = resourceAPI.deleteResource

  private def getModelDefinitionRevisions: Route = resourceAPI.getAllRevisions

  private def createModelDefinitionRevision: Route = resourceAPI.createRevision

  private def downloadModelDefinitionContent: Route = contentAPI.downloadSingleAttachment

  private def uploadModelDefinitionContent: Route = contentAPI.uploadSingleAttachment

}
