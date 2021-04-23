/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.job

import java.util.Date

import akka.Done
import akka.actor.ActorSystem
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.containers.Container
import com.ibm.ml.repository.v4.migration.upgrade.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.upgrade.models._
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.utils.{getContainer, seqFutures}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class MigrationJobsGenerator(sc: MigrationAppContext)(implicit identity: Identity) extends StrictLogging with MigrationConstant {


  implicit val system: ActorSystem = sc.downstreamActorSystem
  implicit val ec: ExecutionContext = system.dispatcher


  case class GlobalSearchPayload(size: Int,
                                 from: Int,
                                 source: Vector[String],
                                 query: JsObject,
                                 sort: Vector[JsObject],
                                 searchAfter: Option[Vector[JsValue]] = None
                                )

  implicit val globalSearchPayloadFormat: RootJsonFormat[GlobalSearchPayload] = jsonFormat(GlobalSearchPayload.apply,
    "size",
    "from",
    "_source",
    "query",
    "sort",
    "search_after"
  )

  private val assetSearchQuery: JsObject = {
    """{
      |    "bool": {
      |      "filter": [
      |        {
      |          "term": {
      |            "provider_type_id": "cams"
      |          }
      |        },
      |        {
      |          "term": {
      |            "metadata.state": "active"
      |          }
      |        },
      |        {
      |          "terms": {
      |            "metadata.artifact_type": [
      |              "wml_model_definition",
      |              "wml_model",
      |              "wml_pipeline",
      |              "wml_function",
      |              "wml_experiment",
      |              "wml_deployment_job_definition"
      |            ]
      |          }
      |        }
      |      ],
      |      "must": {
      |        "bool": {
      |          "should": [
      |            {
      |              "exists": {
      |                "field": "entity.assets.project_id"
      |              }
      |            },
      |            {
      |              "exists": {
      |                "field": "entity.assets.space_id"
      |              }
      |            }
      |          ],
      |          "minimum_should_match": 1
      |        }
      |      }
      |    }
      |  }
      |""".stripMargin.parseJson.asJsObject
  }


  private def generateMigrationDoc(assets: Vector[AssetInfo], spaceId: Option[String], projectId: Option[String]): MigrationDoc = {
    val experimentIds = assets.filter(_.assetType == WML_EXPERIMENT).map(_.assetId)
    val functionIds = assets.filter(_.assetType == WML_FUNCTION).map(_.assetId)
    val modelIds = assets.filter(_.assetType == WML_MODEL).map(_.assetId)
    val pipelineIds = assets.filter(_.assetType == WML_PIPELINE).map(_.assetId)
    val modelDefinitionIds = assets.filter(_.assetType == WML_MODEL_DEFINITION).map(_.assetId)
    val deploymentJobDefinitionIds = assets.filter(_.assetType == WML_DEPLOYMENT_JOB_DEFINITION).map(_.assetId)
    MigrationDoc(
      status = Pending,
      createdAt = new Date().getTime,
      modifiedAt = new Date().getTime,
      results = Some(MigrationResults()),
      console = Some(Seq.empty),
      oldSpaceId = spaceId,
      oldProjectId = projectId,
      newSpaceId = spaceId,
      newProjectId = projectId,
      experimentIds = Some(experimentIds),
      functionIds = Some(functionIds),
      modelIds = Some(modelIds),
      pipelineIds = Some(pipelineIds),
      modelDefinitionIds = Some(modelDefinitionIds),
      deploymentJobDefinitionIds = Some(deploymentJobDefinitionIds),
      skipMigratedAssets = Some(true)
    )
  }

  private def checkSpaceOrProjectExist(identity: Identity,
                                       oldSpaceId: Option[String],
                                       oldProjectId: Option[String],
                                       newSpaceId: Option[String],
                                       newProjectId: Option[String])
                                      (implicit system: ActorSystem, ec: ExecutionContext): Future[Unit] = {
    for {
      _ <- if (oldSpaceId == newSpaceId) {
        oldSpaceId.map(id => sc.authClient.getSpace(identity, id, Map.empty)).getOrElse(Future.successful(Done))
      } else {
        for {
          _ <- oldSpaceId.map(id => sc.authClient.getSpace(identity, id, Map.empty)).getOrElse(Future.successful(Done))
          _ <- newSpaceId.map(id => sc.authClient.getSpace(identity, id, Map.empty)).getOrElse(Future.successful(Done))
        } yield {}
      }
      _ <- if (oldProjectId == newProjectId) {
        oldProjectId.map(id => sc.authClient.getProject(identity, id, Map.empty)).getOrElse(Future.successful(Done))
      } else {
        for {
          _ <- oldProjectId.map(id => sc.authClient.getProject(identity, id, Map.empty)).getOrElse(Future.successful(Done))
          _ <- newProjectId.map(id => sc.authClient.getProject(identity, id, Map.empty)).getOrElse(Future.successful(Done))
        } yield {}
      }
    } yield {}
  }

  private def needMigrate(oldVersion: Option[String], assetMaxVersion: String): Boolean = {
    Try {
      if (oldVersion.isDefined) {
        val oldVersionSplit = oldVersion.get.split("\\.").map(_.toInt)
        val maxVersionSplit = assetMaxVersion.split("\\.").map(_.toInt)
        val length = if (oldVersionSplit.length < maxVersionSplit.length) oldVersionSplit.length else maxVersionSplit.length
        for (i <- 0 until length) {
          if (oldVersionSplit(i) < maxVersionSplit(i)) return true
        }
        false
      } else {
        true
      }
    }.getOrElse(true)
  }


  private def assetNeedMigrate(assetType: String, assetId: String, assetMaxVersion: String, container: Container): Future[Option[String]] = {
    for {
      asset <- sc.camsClient.getAssetMetadata(identity, assetId, container = container, containerStorageType = None)
    } yield {
      val oldVersion: Option[String] = Try(
        asset.entity.flatMap(_.fields.get(assetType)
          .flatMap(_.asJsObject.fields.get("ml_version")
            .map(_.asInstanceOf[JsString].value)))
      ).getOrElse(None)
      if (needMigrate(oldVersion, assetMaxVersion)) {
        Some(assetId)
      } else None
    }
  }


  private def filterByVersion(assetType: String, assetsOpt: Option[Vector[String]], assetMaxVersion: String, container: Container): Future[Option[Vector[String]]] = {
    assetsOpt.map { assets =>
      for {
        assetIds <- seqFutures(assets) { assetId =>
          (for {
            id <- assetNeedMigrate(assetType, assetId, assetMaxVersion, container)
          } yield {
            id
          }) recover (_ => None)
        }
      } yield {
        val results = assetIds.flatten
        if (results.nonEmpty) Some(results) else None
      }
    }.getOrElse(Future.successful(None))
  }

  private def generateDocs(rows: Vector[JsValue], oldJobs: Vector[MigrationDoc]): Future[Vector[Option[MigrationDoc]]] = {
    val migratedAssets = oldJobs.flatMap(_.results).flatMap(_.successful).distinct

    val assetInfos = rows.flatMap { result =>
      Try {
        val jsonMap = result.asJsObject.fields
        val assetType = jsonMap("metadata").asJsObject.fields("artifact_type").asInstanceOf[JsString].value
        val assetId = jsonMap("artifact_id").asInstanceOf[JsString].value
        val assets = jsonMap("entity").asJsObject.fields("assets").asJsObject.fields
        val projectId = assets.get("project_id").map(_.asInstanceOf[JsString].value)
        val spaceId = assets.get("space_id").map(_.asInstanceOf[JsString].value)
        logger.info(s"Found $assetType asset $assetId with project_id = $projectId, space_id = $spaceId")
        AssetInfo(assetId, assetType, projectId, spaceId)
      }.toOption.flatMap { assetInfo =>
        // if the assetId already in the new format we should not migrate again
        if (migratedAssets.exists(assetInfo.assetId == _.newId)) None
        else Some(assetInfo)
      }
    }
    // group by space id and project id
    val (spaceAssets, projectAssets) = (
      assetInfos.filter(_.spaceId.isDefined).groupBy(_.spaceId.get),
      assetInfos.filter(_.projectId.isDefined).groupBy(_.projectId.get)
    )
    val spaceDocs = spaceAssets.iterator.map {
      case (spaceId, assets) =>
        val spaceIdOpt = Some(spaceId)
        generateMigrationDoc(assets, spaceIdOpt, None)
    }.toVector
    val projectDocs = projectAssets.iterator.map {
      case (projectId, assets) =>
        val projectIdOpt = Some(projectId)
        generateMigrationDoc(assets, None, projectIdOpt)
    }.toVector

    val allDocs = spaceDocs ++ projectDocs

    seqFutures(allDocs) { doc =>
      (for {
        _ <- checkSpaceOrProjectExist(identity, doc.oldSpaceId, doc.oldProjectId, doc.newSpaceId, doc.newProjectId)
        container = getContainer(doc.oldSpaceId, doc.oldProjectId)
        experimentIds <- filterByVersion(WML_EXPERIMENT, doc.experimentIds.map(_.toVector), MAX_MIGRATE_VERSION, container)
        functionIds <- filterByVersion(WML_FUNCTION, doc.functionIds.map(_.toVector), MAX_MIGRATE_VERSION, container)
        modelIds <- filterByVersion(WML_MODEL, doc.modelIds.map(_.toVector), MAX_MIGRATE_VERSION, container)
        pipelineIds <- filterByVersion(WML_PIPELINE, doc.pipelineIds.map(_.toVector), MAX_MIGRATE_VERSION, container)
        modelDefinitionIds <- filterByVersion(WML_MODEL_DEFINITION, doc.modelDefinitionIds.map(_.toVector), MAX_MIGRATE_VERSION, container)
        deploymentJobDefinitionIds <- filterByVersion(WML_DEPLOYMENT_JOB_DEFINITION, doc.deploymentJobDefinitionIds.map(_.toVector), MAX_MIGRATE_VERSION, container)
      } yield {
        if(experimentIds.isDefined
          || functionIds.isDefined
          || modelIds.isDefined
          || pipelineIds.isDefined
          || modelDefinitionIds.isDefined
          || deploymentJobDefinitionIds.isDefined) {
          Some(doc.copy(
            experimentIds = experimentIds,
            functionIds = functionIds,
            modelIds = modelIds,
            pipelineIds = pipelineIds,
            modelDefinitionIds = modelDefinitionIds,
            deploymentJobDefinitionIds = deploymentJobDefinitionIds
          ))
        } else None
      }) recover (_ => None)
    }
  }

  private def listAllAssets(identity: Identity): Future[Vector[JsValue]] = {
    def getResult(size: Int, searchAfter: Option[Vector[JsValue]], result: Vector[JsValue]): Future[Vector[JsValue]] = {

      // use search after feature for pagination
      //https://www.elastic.co/guide/en/elasticsearch/reference/current/paginate-search-results.html#search-after

      val payload = GlobalSearchPayload(size, 0, // from always 0
        Vector("artifact_id", "entity.assets", "metadata.artifact_type", "last_updated_at"),
        assetSearchQuery,
        Vector(JsObject(Map("last_updated_at" -> JsString("asc")))),
        searchAfter
      ).toJson

      sc.camsClient.getAssetsByGlobalSearch(identity, payload).flatMap { res =>
        if (res.rows.nonEmpty) {
          val lastUpdatedAt = Try {
            res.rows.last.asJsObject.fields("last_updated_at")
          } match {
            case Success(value) => value
            case Failure(exception) =>
              logger.error("Global search field missing 'last_updated_at' field")
              throw exception
          }
          getResult(size, Some(Vector(lastUpdatedAt)), result ++ res.rows)
        } else {
          Future.successful(result ++ res.rows)
        }
      }
    }

    getResult(1000, None, Vector.empty)
  }

  def getJobs: Future[Vector[MigrationDoc]] = {
    for {
      rows <- listAllAssets(identity)
      oldMigrationJobs <- sc.dbMethods.getAllMigrationDocs
      migrationDocs <- generateDocs(rows, oldMigrationJobs)
      docs <- createCloudantDocument(migrationDocs.flatten)
    } yield {
      docs
    }
  }

  def createCloudantDocument(docs: Vector[MigrationDoc]): Future[Vector[MigrationDoc]] = {
    seqFutures(docs) { doc =>
      sc.dbMethods.client.save(doc.toJson).map { newDoc =>
        newDoc.convertTo[MigrationDoc]
      }
    }
  }
}
