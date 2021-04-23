/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources

import akka.http.scaladsl.model.{StatusCodes, Uri}
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetMetadata, Assets}
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.ml.repository.v4.service.utils.CAMSResourceConverterUtils
import com.ibm.ml.repository.v4.utils.{AssetTypeMismatch, CallContext, FailedConvertCAMSResource, FailedConvertCAMSResourceRevisions, FailedConvertCAMSResources, FailedConvertEntityToCAMS, ServiceException, logPrint}
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait ResourceConverter[EntityRequest, Resource, Resources] extends CAMSResourceConverterUtils {
  def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): Resource

  def addWarningToResource(resource: Resource): Resource = {
    resource
  }

  def convertEntityRequestToJson(entityRequest: EntityRequest): JsValue

  def convertCAMSResource(resource: JsValue, assetType: String): Resource = {
    Try {
      logger.trace(s"start to convert CAMS resource ${logPrint(resource)}")
      val camsAsset = resource.convertTo[Asset]
      val metadata = convertToWMLMetadata(camsAsset.metadata)

      val entityJs = camsAsset.entity.get.fields.getOrElse(assetType, {
        throw ServiceException(StatusCodes.BadRequest, AssetTypeMismatch(assetType, metadata.id, camsAsset.metadata.assetType))
      })

      logger.trace(s"entity JsValue: ${logPrint(entityJs)}")
      addWarningToResource(convertEntityJsonToResource(entityJs, metadata))
    } match {
      case Success(resource) => resource
      case Failure(ex) =>
        ex match {
          case se: ServiceException =>
            logger.error(s"An error occurred inside convertCAMSResource: ${ex.getMessage}")
            throw se
          case e: Throwable =>
            logger.error(s"An error occurred inside convertCAMSResource: ${ex.getMessage}")
            throw ServiceException(StatusCodes.InternalServerError, FailedConvertCAMSResource(ex))
        }
    }
  }

  def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                    next: Option[HyperReference] = None,
                                    limit: Option[Int],
                                    totalRows: Option[Int],
                                    results: Vector[Asset]): Resources

  def convertCAMSResources(resource: JsValue, uri: Uri): Resources = {
    Try {
      logger.trace(s"start to convert CAMS resources ${logPrint(resource)}")
      val camsAssets = resource.convertTo[Assets]
      val totalRows = camsAssets.totalRows.map(_.toInt)
      val results = camsAssets.results
      val (first, limit, next) = getPageInfo(camsAssets, uri)
      convertCAMSResultsToResources(first, next, limit, totalRows, results)
    } match {
      case Success(resources) => resources
      case Failure(ex) =>
        logger.error(s"An error occurred inside convertCAMSResources: ${ex.getMessage}")
        throw ServiceException(StatusCodes.InternalServerError, FailedConvertCAMSResources(ex))
    }
  }

  def convertCAMSResourceRevisions(resource: JsValue, uri: Uri, limit: Option[Int]): CommonRevisionResources = {
    Try {
      logger.trace(s"start to convert CAMS resource revisions ${logPrint(resource)}")
      val results = resource.convertTo[Assets].results
      val (first, next) = getRevisionsPageInfo(results, uri, limit)
      val revisionResources: Vector[CommonRevisionResource] =
        results.map(asset => CommonRevisionResource(convertToWMLMetadata(asset.metadata)))
      // we cannot support total row here due the cams limitation
      CommonRevisionResources(first, next, limit, None, revisionResources)
    } match {
      case Success(resourceRevisions) => resourceRevisions
      case Failure(ex) =>
        logger.error(s"An error occurred inside convertCAMSResourceRevisions: ${ex.getMessage}")
        throw ServiceException(StatusCodes.InternalServerError, FailedConvertCAMSResourceRevisions(ex))
    }
  }

  def addFieldsToCAMSEntity(entity: EntityRequest): JsValue

  def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue

  def getCAMSMetadata(entity: EntityRequest): AssetMetadata

  def convertEntityToCAMS(entity: EntityRequest, assetType: String): JsValue = {
    Try {
      logger.trace(s"start to convert Entity to CAMS resource ${logPrint(convertEntityRequestToJson(entity))}")

      val addedDependencies = addMLVersion(addFieldsToCAMSEntity(entity))
      val metadata = getCAMSMetadata(entity)
      val entityJson = removeMetadataFieldsFromEntity(addedDependencies)
      val entityObject = JsObject(assetType -> entityJson)
      Asset(metadata, Some(entityObject)).toJson
    } match {
      case Success(asset) => asset
      case Failure(ex) =>
        logger.error(s"An error occurred inside convertEntityToCAMS: ${ex.getMessage}")
        throw ServiceException(StatusCodes.BadRequest, FailedConvertEntityToCAMS(ex))
    }
  }

  def convertRevisionEntityToCAMS(entity: CommonRevisionRequest): JsValue = {
    // nothing to convert for now
    entity.toJson
  }

  // we might need to run some async job after the cams doc created,
  // for example upload content for model with import field
  def asyncPostCreationAction(request: EntityRequest, resource: Resource)
                             (implicit callContext: CallContext): Future[Unit] = {
    Future.successful(())
  }

  private def addMLVersion(entityJs: JsValue): JsValue = {
    import DefaultJsonProtocol._
    val fields = entityJs.asJsObject.fields + ("ml_version" -> JsString(getMLVersion))
    fields.toJson
  }

  def getMLVersion: String
}
