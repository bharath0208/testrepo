/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources.validator

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.api.v4ga.models.{ContentInfo, ContentLocation}
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineResource
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.utils.models.ModelTypes
import com.ibm.analytics.wml.utils.queries.{Content, PrivateCloud, PublicCloud}
import com.ibm.ml.repository.v4.service.resources.impl.PipelinesResource
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.service.utils.baseOrDerivedType.baseOrDerivedType
import com.ibm.ml.repository.v4.utils.{CallContext, ContentValidationFailedMessage, InvalidQueryParameterMessage, InvalidRequestEntityMessage, ServiceException, isPrivateCloud, isPublicCloud}
import spray.json.{JsString, JsValue}

import scala.collection.immutable.Map
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Failure

trait ModelsValidator extends ValidateUtils {

  def entityTrainingDataReferencesOptValidator(trainingDataReferences: Option[Vector[DataReference]]): Option[Vector[DataReference]] = {
    if (trainingDataReferences.isEmpty) {
      trainingDataReferences
    } else {
      Some(entityTrainingDataReferencesValidator(trainingDataReferences.get))
    }
  }

  def modelTypeValidator(modelType: String): Unit = {
    val environment = if (isPublicCloud) PublicCloud else PrivateCloud
    if (!ModelTypes.matchType(modelType.toLowerCase, environment)) {
      val msg = s"Unsupported model type '$modelType'"
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }

  def modelSoftwareSpecsValidator(modelSoftwareSpec: SoftwareSpecRef,
                                  pipelineSoftwareSpecs: Option[Vector[SoftwareSpecRef]],
                                  pipeline: Option[ResourceRef],
                                  pipelineClient: PipelinesResource,
                                  envClient: EnvironmentsClient)
                                 (implicit callContext: CallContext,
                                  ec: ExecutionContext): Future[(SoftwareSpecRef, Option[Vector[SoftwareSpecRef]])] = {
    for {
      (softwareSpecNameToIdMap, softwareSpecIdToTypeMap) <- getSoftwareSpecHelperMaps(envClient)
      pipelineSoftwareSpecs <- pipelineSoftwareSpecsValidator(pipelineSoftwareSpecs, pipeline,
        pipelineClient, softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
    } yield {
      val mss = validateSoftwareSpecs(modelSoftwareSpec, softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
      (mss, pipelineSoftwareSpecs)
    }
  }

  def pipelineSoftwareSpecsValidator(pipelineSoftwareSpecs: Option[Vector[SoftwareSpecRef]],
                                     pipeline: Option[ResourceRef],
                                     pipelineClient: PipelinesResource,
                                     softwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                     softwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)])
                                    (implicit callContext: CallContext,
                                     ec: ExecutionContext): Future[Option[Vector[SoftwareSpecRef]]] = {
    if (pipeline.isEmpty) {
      Future.successful(None)
    } else if (pipelineSoftwareSpecs.isDefined && pipelineSoftwareSpecs.get.nonEmpty) {
      Future.successful(pipelineSoftwareSpecs.map { specs =>
        specs.map(sfSpec => {
          validateSoftwareSpecs(sfSpec, softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
        })
      })
    } else {
      val pipelineRef = pipeline.getOrElse(ResourceRef(""))
      import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
      for {
        pipelineResource <- pipelineClient.resourceMethods.get(pipelineRef.id, pipelineRef.rev)
      } yield {
        val result = pipelineResource.convertTo[PipelineResource].entity.document.flatMap { doc =>
          doc.runtimes.map(_.flatMap { runtime =>
            runtime.appData.flatMap(_.wmlData.softwareSpec)
              .map(spec => validateSoftwareSpecs(SoftwareSpecRef(spec), softwareSpecNameToIdMap, softwareSpecIdToTypeMap))
          }.toVector)
        }.getOrElse(Vector.empty)
        if (result.nonEmpty) Some(result)
        else None
      }
    }
  }

  def modelContentLocationValidator(contentLocation: Option[ContentLocation]): Option[ContentLocation] = {
    def requiredFieldWithCondition(requiredField: String, contentType: String, additional: String = "") = {
      val msg = s"'content_location.$requiredField' field is required when the content_location type is $contentType$additional"
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }

    def validateJsMapString(map: Map[String, JsValue], fieldKey: String, fieldName: String): JsString = {
      map.getOrElse(fieldKey, requiredFieldWithCondition(fieldName, S3Type.name)) match {
        case JsString(value) => JsString(stringFieldValidator(value, fieldKey))
        case _ =>
          val msg = s"'$fieldName' must be a String"
          throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }
    }

    def validateContentInfo(contentLocation: ContentLocation): Vector[ContentInfo] = {
      if (contentLocation.contents.isEmpty) {
        emptyArrayError("content_location.contents")
      } else {
        contentLocation.contents.map { content =>
          val contentFormat = stringFieldValidator(content.contentFormat, "content_location.contents[].content_format")
          if (contentFormat == "pipeline-node" && content.pipeLineNodeId.isEmpty) {
            requiredFieldWithCondition("contents[].pipeline_node_id", S3Type.name, " and content_format is pipeline-node")
          }
          content.copy(
            contentFormat = contentFormat,
            location = stringFieldValidator(content.location, "content_location.contents[].location"),
            fileName = stringFieldValidator(content.fileName, "content_location.contents[].file_name"),
            pipeLineNodeId = content.pipeLineNodeId.map { id =>
              stringFieldValidator(id, "content_location.contents[].pipeline_node_id")
            }
          )
        }
      }
    }

    contentLocation.map { contentLocation =>
      val dataSourceType = contentLocation.dataSourceType
      contentLocation.dataSourceType match {
        case ConnectionAssetType | ContainerAssetType | DataAssetType =>
          DataReferenceValidator.validate(contentLocation.dataSourceType.name, contentLocation.connection, contentLocation.location.getOrElse(Map.empty)) match {
            case Failure(exception) =>
              throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(exception.getMessage))
            case _ =>
          }
          val contents = validateContentInfo(contentLocation)
          contentLocation.copy(contents = contents)
        case S3Type =>
          val connection = if (contentLocation.connection.isEmpty) {
            requiredFieldWithCondition("connection", S3Type.name)
          } else {
            connectionValidator(contentLocation.connection)
            contentLocation.connection.map { connection =>
              val endpointUrl = validateJsMapString(connection, "endpoint_url", "connection.endpoint_url")
              val accessKeyId = validateJsMapString(connection, "access_key_id", "connection.access_key_id")
              val secretAccessKey = validateJsMapString(connection, "secret_access_key", "connection.secret_access_key")
              connection + ("endpoint_url" -> endpointUrl) + ("accessKeyId" -> accessKeyId) + ("secret_access_key" -> secretAccessKey)
            }
          }
          val location = if (contentLocation.location.isEmpty) {
            requiredFieldWithCondition("location", S3Type.name)
          } else {
            contentLocation.location.map { location =>
              val bucket = location.getOrElse("bucket", requiredFieldWithCondition("location.bucket", S3Type.name))
              val bucketTrimmed = stringFieldValidator(bucket, "location.bucket")
              location + ("bucket" -> bucketTrimmed)
            }
          }

          val contents = validateContentInfo(contentLocation)
          contentLocation.copy(connection = connection, location = location, contents = contents)
        case FileSystemType if isPrivateCloud =>
          val contents = validateContentInfo(contentLocation)
          contentLocation.copy(contents = contents)
        case _ =>
          val msg = s"Unsupported content location type $dataSourceType"
          throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }
    }
  }

  def isAttachmentsUnique(attachments: Vector[ContentMetadata],
                          modelType: String,
                          contentFormat: String,
                          pipelineNodeID: Option[String],
                          deploymentId: Option[String]): Boolean = {


    val modelTypeLC = modelType.toLowerCase
    val contentFormatLC = contentFormat.toLowerCase
    val environment = if (isPublicCloud) PublicCloud else PrivateCloud

    val content: Option[Content] = ModelTypes.getContent(modelTypeLC, contentFormatLC, environment)

    if (contentFormatLC == "coreml" && deploymentId.isEmpty) {
      val msg = s"deployment_id query param is required when the content_format is 'coreml'"
      throw ServiceException(StatusCodes.BadRequest, ContentValidationFailedMessage(msg))
    }

    if (deploymentId.isDefined && contentFormatLC != "coreml") {
      val msg = s"deployment_id can be used only when the content_format is 'coreml'"
      throw ServiceException(StatusCodes.BadRequest, InvalidQueryParameterMessage("deployment_id", deploymentId.get, Some(msg)))
    }

    val unique: Boolean = content match {
      case Some(content) =>
        content.unique
      case None =>
        val msg = s"Unsupported content_format '$contentFormat' for model_type '$modelType'"
        throw ServiceException(StatusCodes.BadRequest, InvalidQueryParameterMessage("content_format", contentFormat, Some(msg)))
    }

    val replaceNodeId: Boolean = if (pipelineNodeID.isDefined) {
      if (contentFormatLC != "pipeline-node") {
        val msg = s"pipeline_node_id can be used only when the content_format is 'pipeline-node'"
        throw ServiceException(StatusCodes.BadRequest, InvalidQueryParameterMessage("pipeline_node_id", pipelineNodeID.get, Some(msg)))
      } else attachments.nonEmpty
    } else {
      if (contentFormatLC == "pipeline-node") {
        val msg = s"pipeline_node_id query param is required when the content_format is 'pipeline-node'"
        throw ServiceException(StatusCodes.BadRequest, ContentValidationFailedMessage(msg))
      }
      false
    }

    unique || replaceNodeId

  }

  def uploadParamsValidator(contentFormat: String, name: Option[String], pipelineNodeId: Option[String], deploymentId: Option[String]): (String, Option[String], Option[String], Option[String]) = {
    if (contentFormat.trim.isEmpty) {
      blankError("content_format query")
    }
    val nameOpt = name.map(name => {
      if (name.trim.isEmpty) blankError("name query")
      else name.trim
    })

    val pipelineNodeIdOpt = pipelineNodeId.map(pipelineNodeId => {
      if (pipelineNodeId.trim.isEmpty) blankError("pipeline_node_id query")
      else pipelineNodeId.trim
    })

    val deploymentIdOpt = deploymentId.map(deploymentId => {
      if (deploymentId.trim.isEmpty) blankError("deployment_id query")
      else deploymentId.trim
    })

    (contentFormat.trim, nameOpt, pipelineNodeIdOpt, deploymentIdOpt)
  }
}
