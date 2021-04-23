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
import com.ibm.analytics.wml.api.v4ga.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.functions._
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResourceWithContent
import com.ibm.ml.repository.v4.service.resources.validator.FunctionsValidator
import com.ibm.ml.repository.v4.utils.{CallContext, logPrint}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class FunctionsResource(sc: ServiceContext)
                            (implicit system: ActorSystem)
  extends AbstractResourceWithContent[FunctionEntityRequest,
    FunctionResource, FunctionResources](sc) with FunctionsValidator {

  override val name: String = WML_FUNCTION_ASSET_NAME
  override lazy val assetType: String = WML_FUNCTION_ASSET_TYPE
  override val contentAPIName: Option[String] = Some(WML_FUNCTION_CONTENT_API_NAME)

  val uploadContentTypesAllowed: List[String] = List(
    "application/zip",
    "application/gzip",
    "application/octet-stream"
  )

  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllFunctions ~
      createFunction ~
      getFunction ~
      patchFunction ~
      deleteFunction ~
      getFunctionRevisions ~
      createFunctionRevision ~
      getFunctionCode ~
      uploadFunctionCode
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): FunctionResources = {
    val convertedResult: Vector[(Option[FunctionResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    FunctionResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): FunctionResource = {
    val entity = entityJs.convertTo[FunctionEntity]
    FunctionResource(metadata, entity)
  }

  override def addWarningToResource(resource: FunctionResource): FunctionResource = {
    val entity = resource.entity
    val softwareSpecList = Vector(entity.softwareSpec)
    val swSpecMessage = getSoftwareSpecWarningMessages(softwareSpecList, assetType)
    resource.copy(system = addWarningMessages(swSpecMessage, resource.system))
  }


  override def convertEntityRequestToJson(entityRequest: FunctionEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: FunctionEntityRequest): JsValue = {
    // we don't need to add extra field for function
    entity.toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: FunctionEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }

  override def validateEntity(entity: FunctionEntityRequest)
                             (implicit callContext: CallContext): Future[FunctionEntityRequest] = {
    logger.trace(s"Function entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    val functionType = entity.functionType match {
      case Some(value) => Some(value)
      case None => Some(PythonType)
    }
    for {
      swSpecCopy <- softwareSpecValidator(entity.softwareSpec, sc.environmentsClient)
    } yield {
      val entityCopy = entity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId, projectId = trimProjectId,
        softwareSpec = swSpecCopy, functionType = functionType)
      logger.trace(s"Function entity after validation ${logPrint(entityCopy.toJson)}")
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

  override def getMLVersion: String = WML_FUNCTION_ASSET_TYPE_ML_VERSION

  private def getAllFunctions: Route = resourceAPI.getAllResources

  private def createFunction: Route = resourceAPI.createResource

  private def getFunction: Route = resourceAPI.getResource

  private def patchFunction: Route = resourceAPI.patchResource

  private def deleteFunction: Route = resourceAPI.deleteResource

  private def getFunctionRevisions: Route = resourceAPI.getAllRevisions

  private def createFunctionRevision: Route = resourceAPI.createRevision

  private def getFunctionCode: Route = contentAPI.downloadSingleAttachment

  private def uploadFunctionCode: Route = contentAPI.uploadSingleAttachment


}
