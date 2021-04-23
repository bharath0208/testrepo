/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.cams

import akka.http.scaladsl.model.StatusCode
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets._
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.ml.repository.v4.service.endpoints.WMLResourceMethods
import com.ibm.ml.repository.v4.service.utils.AssetConstant
import com.ibm.ml.repository.v4.utils._
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.{ExecutionContext, Future}

/**
 * These are the classes that handle the interaction with the CAMS service.
 */

case class CAMSResourceMethods(camsClient: CAMSClient,
                               assetType: String)
                              (implicit ec: ExecutionContext) extends WMLResourceMethods with StrictLogging with AssetConstant {
  override def getAll(start: Option[String],
                      limit: Option[Int],
                      tagValue: Option[String],
                      search: Option[String])
                     (implicit callContext: CallContext): Future[ResourceClasses] = {
    def getSearchQuery(query: String): String = {
      if (query.startsWith("asset.") || query.startsWith(s"$assetType.")) query
      else {
        s"$assetType.$query"
      }
    }

    val query = (tagValue, search) match {
      case (Some(tag), Some(search)) => Some(s"""$TAGS_QUERY:"$tag" AND ${getSearchQuery(search)}""")
      case (Some(tag), None) => Some(s"$TAGS_QUERY:$tag")
      case (None, Some(search)) => Some(getSearchQuery(search))
      case _ => Some(SEARCH_ALL)
    }
    for {
      res <- camsClient.getAllAssetMetadata(callContext.identity, assetType, start, limit, query, callContext.container, callContext.containerStorageType)
    } yield {
      res.toJson
    }
  }

  override def create(entity: EntityClass)
                     (implicit callContext: CallContext): Future[(ResourceClass, StatusCode)] = {
    var assetInput = entity.convertTo[AssetInput]
    // Remove spaceId and projectId, not allowed in `create` request
    assetInput = assetInput.copy(metadata = assetInput.metadata.copy(spaceId = None, projectId = None))
    for {
      (res, statusCode) <- camsClient.createAsset(callContext.identity, assetInput, callContext.container, callContext.containerStorageType)
    } yield {
      (res.toJson, statusCode)
    }
  }

  override def get(id: String,
                   rev: Option[String])
                  (implicit callContext: CallContext): Future[ResourceClass] = {
    for {
      res <- camsClient.getAssetMetadata(callContext.identity, id, rev, callContext.container, callContext.containerStorageType)
    } yield {
      res.toJson
    }
  }

  override def update(id: String,
                      patch: JsValue)
                     (implicit callContext: CallContext): Future[ResourceClass] = {
    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
    val patchPayload = patch.convertTo[List[PatchPayloadElement]]
    val lists = patchPayload.partition(_.path.startsWith("/metadata"))
    val container = callContext.container
    val cst = callContext.containerStorageType
    def getAsset(asset: Option[Asset]): Future[Asset] = {
      if (asset.isEmpty) camsClient.getAssetMetadata(callContext.identity, id, None, container, cst)
      else Future.successful(asset.get)
    }

    def updateMetadata(): Future[Option[Asset]] =
      if (lists._1.nonEmpty) {
        for {
          result <- camsClient.updateAssetMetadata(callContext.identity, id, lists._1, container, cst)
        } yield {
          Some(result)
        }
      } else
        Future.successful(None)

    def updateAttributes(asset: Option[Asset]): Future[Option[Asset]] =
      if (lists._2.nonEmpty)
        for {
          result <- camsClient.updateAssetAttributes(callContext.identity, id, assetType, lists._2, container, cst)
          res <- getAsset(asset)
        } yield {

          Some(res.copy(entity = Some(Map(assetType -> result.toJson).toJson.asJsObject())))
        }
      else
        Future.successful(None)

    val updateAll: Future[Option[Asset]] = for {
      // here we will try to update in series
      metaResult <- updateMetadata()
      entityResult <- updateAttributes(metaResult)
    } yield {
      (metaResult, entityResult) match {
        case (_, Some(entity)) => Some(entity)
        case (Some(meta), None) => Some(meta)
        case _ => None
      }
    }

    for {
      result <- updateAll
      // if result does not exist we will get the asset
      res <- getAsset(result)
    } yield {
      res.toJson
    }
  }

  override def delete(id: String,
                      purgeOnDelete: Boolean)
                     (implicit callContext: CallContext): Future[Unit] = {
    for {
      _ <- camsClient.deleteAsset(callContext.identity, id, callContext.container, purgeOnDelete, callContext.containerStorageType)
    } yield {}
  }

  override def getAllRevisions(id: String,
                               start: Option[String],
                               limit: Option[Int])
                              (implicit callContext: CallContext): Future[ResourceClasses] = {
    for {
      res <- camsClient.getAllAssetRevisions(callContext.identity, id, start, limit, callContext.container, callContext.containerStorageType)
    } yield {
      res.toJson
    }
  }

  override def createRevision(id: String,
                              entity: RevisionEntityClass)
                             (implicit callContext: CallContext): Future[(ResourceClass, StatusCode)] = {
    val req = entity.convertTo[CommonRevisionRequest]
    val payload = entity.convertTo[RevisionInput]
    for {
      (res, statusCode) <- camsClient.createAssetRevision(callContext.identity, id, payload, callContext.container, callContext.containerStorageType)
    } yield {
      (res.toJson, statusCode)
    }
  }
}
