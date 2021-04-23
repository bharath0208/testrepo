/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.endpoints

import akka.http.scaladsl.model.{StatusCode, StatusCodes, Uri}
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common.{CommonRevisionRequest, PatchPayloadElement}
import com.ibm.analytics.wml.utils.containers.Container
import com.ibm.ml.repository.v4.service.resources.AbstractResource
import com.ibm.ml.repository.v4.utils.{CallContext, MalformedJsonDocumentMessage, ServiceException}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

/**
 * These are the methods that a WML resource supports.
 */
case class AbstractResourceMethods[EntityRequest, Resource, Resources]
(resource: AbstractResource[EntityRequest, Resource, Resources])
(implicit ec: ExecutionContext,
 requestJF: RootJsonFormat[EntityRequest],
 resourceJF: RootJsonFormat[Resource],
 resourcesJF: RootJsonFormat[Resources])
  extends WMLResourceMethods {

  override def getAll(start: Option[String],
                      limit: Option[Int],
                      tagValue: Option[String],
                      search: Option[String])
                     (implicit callContext: CallContext): Future[ResourceClasses] = {
    for {
      camsResources <- resource.camsResource.getAll(start, limit, tagValue, search)
    } yield {
      resource.convertCAMSResources(camsResources,
        Uri(resource.v4ResourcesHref(resource.name, callContext.container, start, limit, tagValue))).toJson
    }
  }

  override def create(entity: EntityClass)
                     (implicit callContext: CallContext): Future[(ResourceClass, StatusCode)] = {
    (for {
      entityReq <- Future.fromTry(Try(entity.convertTo[EntityRequest]))
      entity <- resource.validateEntity(entityReq)
      (camsResource, statusCode) <- resource.camsResource.create(resource.convertEntityToCAMS(entity, resource.assetType))
    } yield {
      val wmlResource = resource.convertCAMSResource(camsResource, resource.assetType)
      resource.asyncPostCreationAction(entity, wmlResource)
      (wmlResource.toJson, statusCode)
    }).recoverWith {
      case de: DeserializationException =>
        Future.failed(ServiceException(StatusCodes.BadRequest, MalformedJsonDocumentMessage(de, s"create ${resource.name} json payload")))
      case e: Throwable =>
        Future.failed(e)
    }
  }

  override def get(id: String,
                   rev: Option[String])
                  (implicit callContext: CallContext): Future[ResourceClass] = {
    for {
      camsResource <- resource.camsResource.get(id, rev)
    } yield {
      resource.convertCAMSResource(camsResource, resource.assetType).toJson
    }
  }

  override def update(id: String,
                      patch: JsValue)
                     (implicit callContext: CallContext): Future[ResourceClass] = {
    (for {
      patchReq <- Future.fromTry(Try(patch.convertTo[List[PatchPayloadElement]]))
      patch <- resource.validatePatchEntity(patchReq)
      camsResource <- resource.camsResource.update(id, patch)
    } yield {
      resource.convertCAMSResource(camsResource, resource.assetType).toJson
    }).recoverWith {
      case de: DeserializationException =>
        Future.failed(ServiceException(StatusCodes.BadRequest, MalformedJsonDocumentMessage(de, s"update ${resource.name} json payload")))
      case e: Throwable =>
        Future.failed(e)
    }
  }

  override def delete(id: String,
                      purgeOnDelete: Boolean)
                     (implicit callContext: CallContext): Future[Unit] = {
    for {
      _ <- resource.camsResource.delete(id, purgeOnDelete)
    } yield {
    }
  }

  override def getAllRevisions(id: String,
                               start: Option[String],
                               limit: Option[Int])
                              (implicit callContext: CallContext): Future[ResourceClasses] = {
    for {
      camsResources <- resource.camsResource.getAllRevisions(id, start, limit)
    } yield {
      resource.convertCAMSResourceRevisions(camsResources, Uri(resource.v4ResourcesHref(resource.name,
        callContext.container, start, limit, assetId = Some(id))), limit).toJson
    }
  }

  override def createRevision(id: String,
                              entity: RevisionEntityClass)
                             (implicit callContext: CallContext): Future[(ResourceClass, StatusCode)] = {
    (for {
      revisionReq <- Future.fromTry(Try(entity.convertTo[CommonRevisionRequest]))
      revisionEntity <- resource.validateRevisionEntity(revisionReq)
      (camsResource, statusCode) <- resource.camsResource.createRevision(id, resource.convertRevisionEntityToCAMS(revisionEntity))
    } yield {
      (resource.convertCAMSResource(camsResource, resource.assetType).toJson, statusCode)
    }).recoverWith {
      case de: DeserializationException =>
        Future.failed(ServiceException(StatusCodes.BadRequest, MalformedJsonDocumentMessage(de, s"create revision ${resource.name} json payload")))
      case e: Throwable =>
        Future.failed(e)
    }
  }
}
