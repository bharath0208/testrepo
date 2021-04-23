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
import akka.stream.scaladsl.Source
import akka.util.ByteString
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetMetadata}
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.api.v4ga.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4ga.models._
import com.ibm.analytics.wml.utils.containers.{ProjectType, SpaceType}
import com.ibm.analytics.wml.utils.models.ModelTypes
import com.ibm.analytics.wml.utils.queries.{PrivateCloud, PublicCloud}
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResourceWithContent
import com.ibm.ml.repository.v4.service.resources.validator.ModelsValidator
import com.ibm.ml.repository.v4.service.store.{CAMSStore, FileStore, FileSystemStore, S3Store}
import com.ibm.ml.repository.v4.utils.logging.AccessLogger
import com.ibm.ml.repository.v4.utils.{InvalidRequestEntityMessage, ServiceException, _}
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class ModelsResource(sc: ServiceContext)
                         (implicit system: ActorSystem)
  extends AbstractResourceWithContent[ModelEntityRequest,
    ModelResource, ModelResources](sc) with ModelsValidator {

  override val name: String = WML_MODEL_ASSET_NAME
  override lazy val assetType: String = WML_MODEL_ASSET_TYPE

  val pipelineClient: PipelinesResource = PipelinesResource(sc)

  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllModels ~
      createModel ~
      getModel ~
      patchModel ~
      deleteModel ~
      getModelRevisions ~
      createModelRevision ~
      getModelContent ~
      uploadModelContent ~
      downloadModelAttachmentWithFilter ~
      downloadModelAttachment ~
      deleteModelAttachment
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): ModelResources = {

    val convertedResult: Vector[(Option[ModelResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    ModelResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): ModelResource = {
    val entity = {
      val ent = entityJs.convertTo[ModelEntity]
      // this should not be needed as deployments should not be based on the json but should
      // do the same test as below? https://github.ibm.com/NGP-TWC/ml-planning/issues/18482
      ent.schemas match {
        case Some(schemas) if schemas.input.isEmpty && schemas.output.isEmpty =>
          ent.copy(schemas = None)
        case _ =>
          ent
      }
    }
    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
    val system = entityJs.asJsObject.fields.get("system").map(_.convertTo[SystemDetails])
    ModelResource(metadata, entity, system)
  }

  override def addWarningToResource(resource: ModelResource): ModelResource = {

    val typeMessage = modelTypeDeprecatedWarning(resource)
    val swSpecMessage = SWSpecDeprecatedWarning(resource)
    val messages: Vector[String] = typeMessage ++ swSpecMessage
    resource.copy(system = addWarningMessages(messages, resource.system))
  }

  private def modelTypeDeprecatedWarning(resource: ModelResource): Vector[String] = {

    val modelTypesEnv = if (isPrivateCloud) PrivateCloud else PublicCloud
    val modelType = resource.entity.modelType
    if (ModelTypes.isDeprecated(resource.entity.modelType, modelTypesEnv)) {
      Vector(s"Model type $modelType is deprecated. We recommend you use a supported model type. See Supported Frameworks $getSupportedFrameworksUrl")
    } else Vector.empty
  }

  private def SWSpecDeprecatedWarning(resource: ModelResource): Vector[String] = {
    val entity = resource.entity
    val softwareSpecList = entity.pipelineSoftwareSpecs.getOrElse(Vector.empty).appended(entity.softwareSpec)
    getSoftwareSpecWarningMessages(softwareSpecList, assetType)
  }

  override def addFieldsToCAMSEntity(entity: ModelEntityRequest): JsValue = {
    import DefaultJsonProtocol._
    implicit val swSpecFormat: RootJsonFormat[SoftwareSpecRef] = CommonJsonFormat.defaultSoftwareSpecRefFormat
    implicit val modelEntityRequestFormat: RootJsonFormat[ModelEntityRequest] = com.ibm.analytics.wml.api.v4ga.models.ModelJsonFormat.modelEntityRequestFormat
    val jsFields = entity.toJson.asJsObject.fields
    val dataAssetDependencies: Vector[String] = entity.trainingDataReferences.map(tdr =>
      getDataAssetsIdsFromDataRefs(tdr)
    ).getOrElse(Vector())
    val dataAssetDependenciesMap = getListOpt(dataAssetDependencies, DATA_ASSET_DEPENDENCIES).getOrElse(Map())

    val contentImportStateMap = entity.contentLocation.map { _ =>
      val state: ContentImportState = ImportRunningState
      Map(CONTENT_IMPORT_STATE -> state.toJson)
    }.getOrElse(Map.empty)

    val updatedModelEntity = jsFields ++ dataAssetDependenciesMap ++ contentImportStateMap
    updatedModelEntity.toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def convertEntityRequestToJson(entityRequest: ModelEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def getCAMSMetadata(entity: ModelEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }

  override def validateEntity(entity: ModelEntityRequest)
                             (implicit callContext: CallContext): Future[ModelEntityRequest] = {
    logger.trace(s"Model entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", blankCheck = true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val modelType = stringFieldValidator(entity.modelType, "type").toLowerCase
    // check model type
    modelTypeValidator(modelType)

    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    val trainingDataReferences = entityTrainingDataReferencesOptValidator(entity.trainingDataReferences)
    val contentLocation = modelContentLocationValidator(entity.contentLocation)

    if (entity.pipelineSoftwareSpecs.isDefined && contentLocation.isEmpty) {
      val msg = s"'hybrid_pipeline_software_specs' can be overwritten only when the `content_location` field is provided"
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }

    for {
      (swSpecFtr, pipelineSoftwareSpecs) <- modelSoftwareSpecsValidator(
        entity.softwareSpec,
        entity.pipelineSoftwareSpecs,
        entity.pipeline,
        pipelineClient,
        sc.environmentsClient)
    } yield {
      val entityCopy = entity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId,
        projectId = trimProjectId, modelType = modelType, softwareSpec = swSpecFtr, trainingDataReferences = trainingDataReferences,
        contentLocation = contentLocation, pipelineSoftwareSpecs = pipelineSoftwareSpecs)
      logger.trace(s"Model entity after validation ${logPrint(entityCopy.toJson)}")
      entityCopy
    }
  }

  override def validateUpload(resource: JsValue,
                              contentFormat: String,
                              contentMimeType: String,
                              name: Option[String],
                              pipelineNodeId: Option[String],
                              deploymentId: Option[String])
                             (implicit callContext: CallContext): Future[(String, Option[String], Option[String], Option[String])] = {
    val wmlResource = convertCAMSResource(resource, assetType)
    val (assetId, attachments) = extractInfoFromAsset(resource.convertTo[Asset])

    val (trimmedContentFormat, trimmedName, trimmedPipelineNodeIdOpt, trimmedDeploymentOpt) = uploadParamsValidator(contentFormat, name, pipelineNodeId, deploymentId)

    val modelType = wmlResource.entity.modelType

    validateUploadContentTypeForModelType(contentMimeType, modelType)
    val filteredAttachments = filterContentMetadata(Some(contentFormat), None, trimmedPipelineNodeIdOpt, None, attachments)
    val singleAttachment = isAttachmentsUnique(filteredAttachments, modelType, trimmedContentFormat, trimmedPipelineNodeIdOpt, trimmedDeploymentOpt)

    if (singleAttachment) {
      for {
        _ <- deleteAttachments(assetId, filteredAttachments.map(_.attachmentId), sc.camsClient)
      } yield {
        (trimmedContentFormat, trimmedName, trimmedPipelineNodeIdOpt, trimmedDeploymentOpt)
      }
    } else {
      Future.successful((trimmedContentFormat, trimmedName, trimmedPipelineNodeIdOpt, trimmedDeploymentOpt))
    }
  }

  private def validateUploadContentTypeForModelType(contentMIMEType: String, modelType: String): Unit = {
    val environment = if (isPublicCloud) PublicCloud else PrivateCloud
    val modelContentTypes = ModelTypes.contentTypes(modelType, environment).flatMap(_.types).flatMap(_.mimeTypes)
    if (!modelContentTypes.contains(contentMIMEType)) {
      if (enableContentCheck) {
        contentTypeNotAllowedForModelTypeError(contentMIMEType, modelType, modelContentTypes.mkString(", "))
      }
    }
  }

  private def logDataAccess(fileStore: FileStore,
                            contentLocation: String,
                            action: String,
                            method: String,
                            startTime: Long,
                            failure: Option[Throwable])
                           (implicit callContext: CallContext): Unit = {
    failure match {
      case Some(exception) =>
        AccessLogger.logDataAccess(
          identity = callContext.identity,
          action = action,
          downstream = fileStore.storeType,
          host = fileStore.host,
          port = fileStore.port,
          method = method,
          uri = fileStore.uri(contentLocation),
          startTime = startTime,
          duration = System.currentTimeMillis() - startTime,
          requestId = callContext.identity.requestId,
          failure = Some(exception),
          callerIP = callContext.getClientIp
        )
      case None =>
        AccessLogger.logDataAccess(
          identity = callContext.identity,
          action = action,
          downstream = fileStore.storeType,
          host = fileStore.host,
          port = fileStore.port,
          method = method,
          uri = fileStore.uri(contentLocation),
          startTime = startTime,
          duration = System.currentTimeMillis() - startTime,
          requestId = callContext.identity.requestId,
          failure = None,
          callerIP = callContext.getClientIp
        )
    }
  }

  override def asyncPostCreationAction(request: ModelEntityRequest,
                                       resource: ModelResource)
                                      (implicit callContext: CallContext): Future[Unit] = {
    if (request.contentLocation.isEmpty) {
      Future.successful(())
    } else {
      val startTime = System.currentTimeMillis()
      val contentLocation = request.contentLocation.get
      val dataSourceType = contentLocation.dataSourceType
      val connection = contentLocation.connection
      val location = contentLocation.location
      val assetId = resource.metadata.id
      val container = callContext.container

      (for {
        fileStore <- getFileStore(dataSourceType, connection, location)
        _ <- {
          var fAccum: Future[Unit] = Future {}
          for (content <- contentLocation.contents) {
            logger.info(s"Start to upload content ${content.fileName}")
            fAccum = fAccum flatMap { _ =>
              for {
                source <- getContentBinary(fileStore, content) recoverWith {
                  case t: Throwable =>
                    logger.error(s"Failed to fetch model content ${content.fileName} from ${fileStore.storeType} due to error ${t.getMessage}")
                    logDataAccess(fileStore, content.location, "read", "GET", startTime, Some(t))
                    throw t
                }
                _ <- contentMethods.get.uploadAttachment(assetId = assetId,
                  assetType = assetType,
                  dataSource = source,
                  contentFormat = content.contentFormat,
                  contentMIMEType = detectFileMimeType(content.contentFormat, request.modelType, content.fileName, resource.metadata.id),
                  name = Some(content.fileName),
                  pipelineNodeId = content.pipeLineNodeId,
                  deploymentId = content.deploymentId)
              } yield {
                logDataAccess(fileStore, content.location, "read", "GET", startTime, None)
              }
            }
          }
          fAccum
        }
        _ <- {
          val state: ContentImportState = ImportCompletedState
          import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
          val patch = List(PatchPayloadElement(op = "replace", path = s"/$CONTENT_IMPORT_STATE", value = Some(state.toJson))).toJson
          camsResource.update(assetId, patch)
        }
      } yield {
        logger.info(s"Successfully import the model contents for the asset $assetId")
      }) recoverWith {
        case t: Throwable =>
          val msg = s"Failed to import model contents for asset $assetId due to error: ${t.getMessage}"
          logger.error(msg)
          import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
          val system = SystemDetails(Some(Vector(Warning(None, msg))))
          val state: ContentImportState = ImportFailedState
          val patch = List(
            PatchPayloadElement(op = "add", path = "/system", value = Some(system.toJson)),
            PatchPayloadElement(op = "replace", path = s"/$CONTENT_IMPORT_STATE", value = Some(state.toJson))
          ).toJson
          camsResource.update(assetId, patch)
          throw t
      }
    }
  }

  private def detectFileMimeType(contentFormat: String, modelType: String, fileName: String, modelId: String): String = {
    val environment = if (isPublicCloud) PublicCloud else PrivateCloud
    val allowedTypes = ModelTypes.contentTypes(modelType, environment).find(_.format == contentFormat).map(_.types)
    allowedTypes.flatMap(_.find(t => fileName.endsWith(t.`type`)).map(_.mimeTypes.head)).getOrElse(
      if (enableContentCheck)
        contentTypeNotAllowedForModelTypeError(fileName, modelType, allowedTypes.mkString(", "))
      else "application/octet-stream"
    )
  }

  private def getContentBinary(fileStore: FileStore,
                               contentInfo: ContentInfo)
                              (implicit callContext: CallContext): Future[Source[ByteString, Any]] = {
    fileStore.read(contentInfo.location)
  }


  private def getFileStore(dataSourceType: DataSourceType,
                           connection: Option[Map[String, JsValue]],
                           location: Option[Map[String, String]])(implicit callContext: CallContext): Future[FileStore] = {


    dataSourceType match {
      case DataAssetType =>
        Future {
          val assetId = connection.get("id").asInstanceOf[JsString].value
          val rev = connection.get.get("rev").map(_.asInstanceOf[JsString].value)
          CAMSStore(sc.camsClient, callContext.identity, assetId, rev, callContext.container, callContext.containerStorageType)
        }
      case ConnectionAssetType =>
        def missingInfo(connectionId: String, name: String) = {
          val msg = s"Connection asset $connectionId missing $name information"
          throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(msg))
        }

        val connectionId = connection.get("id").asInstanceOf[JsString].value
        for {
          meta <- sc.camsClient.getConnectionMetadata(callContext.identity, connectionId, callContext.container)
        } yield {
          val entity = meta.entity.getOrElse(missingInfo(connectionId, "entity"))
          val endpointUrl = entity.properties.fields.getOrElse("url", missingInfo(connectionId, "url")).asInstanceOf[JsString].value
          val accessKeyId = entity.properties.fields.getOrElse("access_key", missingInfo(connectionId, "access_key")).asInstanceOf[JsString].value
          val secretAccessKey = entity.properties.fields.getOrElse("secret_key", missingInfo(connectionId, "secret_key")).asInstanceOf[JsString].value
          val bucket = location.get.getOrElse("bucket", entity.properties.fields.getOrElse("bucket", missingInfo(connectionId, "bucket")).asInstanceOf[JsString].value)
          S3Store(accessKey = accessKeyId, secretKey = secretAccessKey, url = endpointUrl, bucket = bucket)
        }
      case ContainerAssetType =>
        def missingInfo(name: String) = {
          val msg = s"${callContext.container.containerType.name} ${callContext.container.id} missing $name information"
          throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(msg))
        }

        if (isPrivateCloud) {
          Future {
            FileSystemStore(trainingResultRootPath)
          }
        } else {

          for {
            (endpointUrl, accessKeyId, secretAccessKey, bucket) <- callContext.container.containerType match {
              case ProjectType =>
                for {
                  meta <- sc.containerClient.getProject(callContext.identity, callContext.container.id)
                } yield {
                  val properties = meta.entity.storage.properties.getOrElse {
                    missingInfo("storage.properties")
                  }
                  val endpointUrl = properties.endpointUrl
                  val bucket = properties.bucketName
                  val credential = properties.credentials.admin.getOrElse(properties.credentials.editor.getOrElse(properties.credentials.viewer))
                  val accessKeyId = credential.accessKeyId.getOrElse(missingInfo("access_key_id"))
                  val secretAccessKey = credential.secretAccessKey.getOrElse(missingInfo("secret_access_key"))
                  (endpointUrl, accessKeyId, secretAccessKey, bucket)
                }
              case SpaceType =>

                for {
                  meta <- sc.containerClient.getSpace(callContext.identity, callContext.container.id)
                } yield {
                  val properties = meta.entity.storage.getOrElse {
                    missingInfo("storage")
                  }.properties
                  val endpointUrl = properties.endpointUrl
                  val bucket = properties.bucketName
                  val credential = properties.credentials.admin.getOrElse(properties.credentials.editor.getOrElse(properties.credentials.viewer))
                  val accessKeyId = credential.accessKeyId.getOrElse(missingInfo("access_key_id"))
                  val secretAccessKey = credential.secretAccessKey.getOrElse(missingInfo("secret_access_key"))
                  (endpointUrl, accessKeyId, secretAccessKey, bucket)
                }
            }
          } yield {
            S3Store(accessKey = accessKeyId, secretKey = secretAccessKey, url = endpointUrl, bucket = bucket)
          }
        }
      case S3Type =>
        for {
          fileStore <- Future.fromTry(Try {
            val endpointUrl = connection.get("endpoint_url").asInstanceOf[JsString].value
            val accessKeyId = connection.get("access_key_id").asInstanceOf[JsString].value
            val secretAccessKey = connection.get("secret_access_key").asInstanceOf[JsString].value
            val bucket = location.get("bucket")
            S3Store(accessKey = accessKeyId, secretKey = secretAccessKey, url = endpointUrl, bucket = bucket)
          })
        } yield {
          fileStore
        }
      case FileSystemType if isPrivateCloud =>
        Future {
          FileSystemStore(trainingResultRootPath)
        }
      case _ =>
        val msg = s"Unsupported content location type $dataSourceType"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }

  override def getMLVersion: String = WML_MODEL_ASSET_TYPE_ML_VERSION

  private def getAllModels: Route = resourceAPI.getAllResources

  private def createModel: Route = resourceAPI.createResource

  private def getModel: Route = resourceAPI.getResource

  private def patchModel: Route = resourceAPI.patchResource

  private def deleteModel: Route = resourceAPI.deleteResource

  private def getModelRevisions: Route = resourceAPI.getAllRevisions

  private def createModelRevision: Route = resourceAPI.createRevision

  private def getModelContent: Route = contentAPI.getContent

  private def uploadModelContent: Route = contentAPI.uploadAttachment

  private def downloadModelAttachmentWithFilter: Route = contentAPI.downloadAttachmentWithFilter

  private def downloadModelAttachment: Route = contentAPI.downloadAttachment

  private def deleteModelAttachment: Route = contentAPI.deleteAttachment
}
