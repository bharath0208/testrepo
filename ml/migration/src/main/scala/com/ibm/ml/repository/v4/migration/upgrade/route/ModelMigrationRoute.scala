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

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{ContentType, ContentTypes, MediaTypes}
import akka.stream.scaladsl.{Sink, Source}
import akka.util.ByteString
import com.ibm.analytics.wml.api.v4.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4.models.ModelResource
import com.ibm.analytics.wml.api.v4ga.models.ModelEntityRequest
import com.ibm.analytics.wml.api.{v4, v4ga}
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.models.ModelTypes
import com.ibm.analytics.wml.utils.specs.{PipelineDef, PipelineDoc}
import com.ibm.ml.repository.v4.migration.upgrade.converters.ModelConverter
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds, MigrationResults}
import com.ibm.ml.repository.v4.migration.utils.MigrationConfig._
import com.ibm.ml.repository.v4.utils.isPublicCloud
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class ModelMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[ModelResource, ModelEntityRequest](mj)(system,
    identity,
    v4.models.ModelJsonFormat.modelResourceFormat,
    v4ga.models.ModelJsonFormat.modelEntityRequestFormat
  ) with ModelConverter {

  override def oldAssetType: String = MODELS_TYPE

  override def newAssetType: String = MODELS_TYPE

  override protected def getOldAssetAction(id: String, doc: MigrationDoc): Future[(ModelResource, Option[String], Seq[String])] = {
    for {
      resource <- mj.sc.v4BetaRepositoryClient.models.get(identity, id, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
    } yield {
      (resource, None, Seq.empty)
    }
  }

  override protected def getDependenciesAction(oldResource: ModelResource, doc: MigrationDoc): Future[MigrationIds] = {
    val entity = oldResource.entity
    val pipelineIds = entity.pipeline.map(_.getId()).map(Seq(_)).getOrElse(Seq.empty)
    val trainingLibIds = entity.trainingLib.map(_.getId()).map(Seq(_)).getOrElse(Seq.empty)
    val runtimeIds = entity.runtime.map { runtime =>
      getRuntimeId(runtime.getId(), oldResource.entity.modelType)
    }.map(Seq(_)).getOrElse(Seq.empty)
    val modelDefIds = entity.modelDefinition.flatMap(_.id).map(Seq(_)).getOrElse(Seq.empty)

    Future.successful(MigrationIds(pipelineIds = pipelineIds, trainingLibraryIds = trainingLibIds, runtimeIds = runtimeIds, modelDefinitionIds = modelDefIds))
  }

  private def getRuntimeId(id: String, modelType: String): String = {
    //special case for the xgboost, this is due to a bug from old repo
    if (id.contains("xgboost_0.80")) {
      if (modelType.contains("0.6") || modelType.contains("0.80") || modelType.contains("0.82") || modelType.contains("0.90")) {
        s"$modelType-py3.6"
      } else "xgboost_0.82-py3.6"
    } else id
  }

  override protected def convertToRequestAction(oldResource: ModelResource, dependencies: MigrationIds, doc: MigrationDoc): Future[(ModelEntityRequest, Seq[String])] = {
    val entity = oldResource.entity
    val metadata = oldResource.metadata

    val oldEntity = entity.toJson.asJsObject.fields - FIELD_TAGS - FIELD_SPACE - FIELD_NAME -
      FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_PIPELINE - FIELD_TRAINING_LIB - FIELD_MODEL_DEFINITION -
      FIELD_RUNTIME - FIELD_IMPORT - FIELD_CONTENT_STATUS - FIELD_CONTENT_FORMAT - FIELD_TRAINING_DATA_REFERENCES


    val newName = getNewName(entity.name, oldAssetType, metadata.id)
    val newIds = addSpaceOrProjectId(doc.newSpaceId, doc.newProjectId)
    val newTags = convertTags(entity.tags)


    val defaultSoftwareSpec = entity.softwareSpec.map { spec =>
      upgradeSoftwareSpec(mj.softwareSpecs, spec.id, spec.name)
    }.getOrElse(getBaseSoftwareSpecByName(mj.softwareSpecs, DEFAULT_MODEL_SOFTWARE_SPEC))
    val (softwareSpecRef, consoleMessages) = Try {
      entity.runtime.map { rr =>
        val oldRId = getRuntimeId(rr.getId(), oldResource.entity.modelType)
        val newId = findResultId(oldRId, RUNTIMES_TYPE, doc.results.getOrElse(MigrationResults()), doc.mapping)
        import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
        jsonMap(FIELD_SOFTWARE_SPEC, v4ga.common.ResourceRef(id = newId).toJson)
      }.getOrElse(defaultSoftwareSpec)
    } match {
      case Success(value) => (value, Seq.empty)
      case Failure(exception) =>
        val msg = s"Unable to convert the runtime for model asset ${metadata.id} due to ${exception.getMessage} to a software specification. Using default software spec $DEFAULT_MODEL_SOFTWARE_SPEC instead"
        logger.error(msg)
        val consoleMsgs = Seq(s"Warning: $msg. If the deployment fails, create the asset manually.")
        (defaultSoftwareSpec, consoleMsgs)
    }

    val pipeline = convertResourceRef(
      entity.pipeline,
      PIPELINES_TYPE,
      FIELD_PIPELINE,
      doc.results.getOrElse(MigrationResults()),
      doc.mapping)


    val modelDef: Map[String, JsValue] = if (entity.trainingLib.isDefined) {
      convertResourceRef(
        entity.trainingLib,
        TRAINING_LIBS_TYPE,
        FIELD_MODEL_DEFINITION,
        doc.results.getOrElse(MigrationResults()),
        doc.mapping)
    } else if (entity.modelDefinition.isDefined) {
      convertResourceRef(
        entity.modelDefinition,
        MODEL_DEFINITIONS_TYPE,
        FIELD_MODEL_DEFINITION,
        doc.results.getOrElse(MigrationResults()),
        doc.mapping)
    } else Map.empty


    val trainingDataReferences = oldResource.entity.trainingDataReferences.map { tdr =>
      val newReferences = tdr.filter { ref =>
        val dType = ref.dataSourceType
        val result = !dType.equalsIgnoreCase("unknown") &&
          !dType.equalsIgnoreCase("dashdb")
        result
      }
      import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
      if (newReferences.isEmpty) Map.empty
      else jsonMap(FIELD_TRAINING_DATA_REFERENCES, newReferences.toJson)
    }.getOrElse(Map.empty)

    val modelTypeUpgradeMap = modelTypeUpgradeCommonMap ++ (if(isPublicCloud) modelTypeUpgradePublicMap else modelTypeUpgradePrivateMap)
    val modelType = {
      val oldType = oldResource.entity.modelType
      modelTypeUpgradeMap.get(oldType).map(newType => {
        jsonMap(FIELD_TYPE, newType.toJson)
      }).getOrElse(jsonMap(FIELD_TYPE, oldType.toJson))
    }

    import com.ibm.analytics.wml.api.v4ga.models.ModelJsonFormat._
    val request = (oldEntity ++ newName ++ newIds ++ newTags ++ softwareSpecRef ++ pipeline ++ modelDef ++ trainingDataReferences ++ modelType).toJson.convertTo[ModelEntityRequest]
    Future.successful((request, consoleMessages))
  }

  override protected def createNewAssetAction(request: ModelEntityRequest, oldResource: ModelResource, doc: MigrationDoc): Future[String] = {
    val entity = oldResource.entity
    val oldId = oldResource.metadata.id
    val projectId = doc.newProjectId
    val spaceId = doc.newSpaceId


    for {
      (resource, _) <- mj.sc.mlRepositoryClient.models.create(identity, ML_REPO_VERSION, request.copy(custom = addMigratedFrom(request.custom, oldResource.metadata.id, doc)))
      _ <- {
        val newId = resource.metadata.id
        val contentFormats = entity.contentFormats.getOrElse(Vector.empty)
        val uploadContentFormatsList = {
          var fAccum: Future[Unit] = Future {}
          for (format <- contentFormats) {
            logger.info(s"Start to upload content format $format")
            val deducedContentType: Option[String] = ModelTypes.getContent(oldResource.entity.modelType, format).map(_.types.head.mimeTypes.head)
            val contentType: ContentType = deducedContentType.map(ContentType.parse(_) match {
              case Left(_) => ContentTypes.`application/octet-stream`
              case Right(cType) => cType
            }).getOrElse(ContentTypes.`application/octet-stream`)
            fAccum = fAccum flatMap { _ =>
              uploadModelSingleContent(oldId, newId,
                oldFormat = Some(format),
                contentFormat = format,
                contentType = contentType,
                doc = doc
              )
            }
          }
          fAccum
        }
        import com.ibm.analytics.wml.utils.specs.PipelineJsonFormats._
        val uploadList = if (entity.modelType.contains(MODEL_TYPE_HYBRID)) {
          for {
            source <- mj.sc.v4BetaRepositoryClient.models.download(identity, oldId, artifact = Some(ARTIFACT_PIPELINE_MODEL), spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
            jsString <- readStream(source)
            pipelineJs = jsString.parseJson.convertTo[PipelineDoc]
            //_ = logger.debug(s"Pipeline doc for the old hybrid model: $jsString" )
            _ <- {
              pipelineJs.pipelines.headOption.map { pipeline =>
                pipelineJs.runtimes.map { runtimes =>
                  val autoAIID = runtimes.find(_.name == AUTO_AI_KB).map(_.id)
                  val obmID = runtimes.find(_.name == AUTO_AI_OBM).map(_.id)

                  val patchedPipelineDef: PipelineDef = autoAIID.map(id => pipeline.nodes.find(_.runtimeRef.contains(id)) match {
                    case Some(_) => pipeline
                    case None =>
                      // in this case we are missing runtime ref for auto ai, need to patch the old content
                      val patchedNodes = if (pipeline.nodes.nonEmpty) {
                        pipeline.nodes.updated(0, pipeline.nodes.head.copy(runtimeRef = autoAIID))
                      } else pipeline.nodes
                      pipeline.copy(nodes = patchedNodes)
                  }).getOrElse(pipeline)
                  //logger.debug(s"Pipeline def for the hybrid model after patch : ${patchedPipelineDef.toJson.compactPrint}" )
                  for {
                    // autoAI upload
                    _ <- autoAIID.flatMap(id => patchedPipelineDef.nodes.find(_.runtimeRef.contains(id))).map(_.id).map { nodeId =>
                      uploadModelSingleContent(oldId, newId,
                        contentFormat = CONTENT_TYPE_PIPELINE_NODE,
                        pipelineNodeId = Some(nodeId),
                        doc = doc)
                    }.getOrElse(Future.successful(()))
                    // obm upload
                    _ <- obmID.flatMap(id => patchedPipelineDef.nodes.find(_.runtimeRef.contains(id))).map(_.id).map { nodeId =>
                      uploadModelSingleContent(oldId, newId,
                        // obm the artifact name need to be the node id
                        artifact = Some(nodeId),
                        contentFormat = CONTENT_TYPE_PIPELINE_NODE,
                        pipelineNodeId = Some(nodeId),
                        doc = doc)
                    }.getOrElse(Future.successful(()))
                    // upload pipeline model
                    _ <- uploadPipelineModelContent(oldId, newId,
                      artifact = Some(ARTIFACT_PIPELINE_MODEL),
                      contentFormat = CONTENT_TYPE_NATIVE,
                      content = Source.single(ByteString.fromString(
                        pipelineJs.copy(pipelines = List(patchedPipelineDef)).toJson.prettyPrint)),
                      contentType = ContentTypes.`application/json`,
                      spaceId = spaceId,
                      projectId = projectId
                    )

                  } yield {}
                }.getOrElse(Future.successful(()))
              }.getOrElse(Future.successful(()))
            }
          } yield {


          }
        } else {

          for {
            _ <- uploadModelSingleContent(oldId, newId,
              CONTENT_TYPE_NATIVE,
              doc = doc)
            // this is a hack fix for the core ML model upload, the problem is due to an old repo bug
            _ <- uploadModelSingleContent(oldId, newId,
              oldFormat = Some(CONTENT_TYPE_CORE_ML),
              contentFormat = CONTENT_TYPE_CORE_ML,
              contentType = if (entity.modelType == "pmml") MediaTypes.`application/xml`.withMissingCharset else ContentTypes.`application/octet-stream`,
              doc = doc
            )
          } yield {

          }

        }
        for {
          _ <- uploadList
          _ <- uploadContentFormatsList
        } yield {}
      }
    } yield {
      resource.metadata.id
    }
  }

  private def readStream(source: Source[ByteString, Any]): Future[String] = {
    val sink = Sink.fold[String, ByteString]("") { case (acc, str) =>
      acc + str.decodeString("US-ASCII")
    }
    source.runWith(sink)
  }


  private def uploadPipelineModelContent(oldId: String,
                                         newId: String,
                                         contentFormat: String,
                                         artifact: Option[String] = None,
                                         content: Source[ByteString, Any],
                                         contentType: ContentType,
                                         spaceId: Option[String],
                                         projectId: Option[String]): Future[Unit] = {

    (for {
      _ <- mj.sc.mlRepositoryClient.models.upload(identity,
        ML_REPO_VERSION,
        newId,
        None,
        contentFormat = contentFormat,
        content = content,
        contentType = contentType,
        projectId = projectId,
        spaceId = spaceId
      )
    } yield {}) recoverWith {
      case e: Throwable =>
        // if the model does not contains content, we will ignore the error
        val msg = s"Skip to migrate the content artifact ${artifact.getOrElse("")} for the model asset $oldId: ${e.getMessage}"
        logger.info(msg)
        Future.successful(())
    }

  }

  private def uploadModelSingleContent(oldId: String,
                                       newId: String,
                                       contentFormat: String,
                                       artifact: Option[String] = None,
                                       oldFormat: Option[String] = None,
                                       pipelineNodeId: Option[String] = None,
                                       doc: MigrationDoc,
                                       contentType: ContentType = ContentTypes.`application/octet-stream`): Future[Unit] = {

    (for {
      source <- mj.sc.v4BetaRepositoryClient.models.download(identity, oldId, artifact = artifact, format = oldFormat, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
      _ <- mj.sc.mlRepositoryClient.models.upload(identity,
        ML_REPO_VERSION,
        newId,
        None,
        contentFormat = contentFormat,
        pipelineNodeId = pipelineNodeId,
        content = source,
        contentType = contentType,
        projectId = doc.newProjectId,
        spaceId = doc.newSpaceId
      )
    } yield {}) recoverWith {
      case e: Throwable =>
        // if the model does not contains content, we will ignore the error
        val msg = s"Skip to migrate the content artifact ${artifact.getOrElse("")} for the model asset $oldId: ${e.getMessage}"
        logger.info(msg)
        Future.successful(())
    }

  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.models.get(
      identity = identity,
      version = ML_REPO_VERSION,
      id = newId,
      spaceId = doc.newSpaceId,
      projectId = doc.newProjectId)
  }
}
