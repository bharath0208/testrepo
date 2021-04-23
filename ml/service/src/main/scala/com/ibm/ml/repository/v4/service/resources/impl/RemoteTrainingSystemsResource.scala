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
import com.ibm.analytics.wml.api.v4ga.common.{HyperReference, Metadata, PatchPayloadElement, Warning}
import com.ibm.analytics.wml.api.v4ga.remote_training_systems.RemoteTrainingSystemJsonFormat._
import com.ibm.analytics.wml.api.v4ga.remote_training_systems._
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.AbstractResource
import com.ibm.ml.repository.v4.service.resources.validator.RemoteTrainingSystemsValidator
import com.ibm.ml.repository.v4.utils.{CallContext, logPrint}
import spray.json._

import scala.collection.immutable.Map
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class RemoteTrainingSystemsResource(sc: ServiceContext)
                                        (implicit system: ActorSystem)
  extends AbstractResource[RemoteTrainingSystemEntityRequest,
    RemoteTrainingSystemResource, RemoteTrainingSystemResources](sc) with RemoteTrainingSystemsValidator {

  override val name: String = WML_REMOTE_TRAINING_SYSTEM_ASSET_NAME
  override lazy val assetType: String = WML_REMOTE_TRAINING_SYSTEM_ASSET_TYPE
  // the endpoints defined in this class
  override val endpoints: Route = {
    getAllRemoteTrainingSystems ~
      createRemoteTrainingSystem ~
      getRemoteTrainingSystem ~
      patchRemoteTrainingSystem ~
      deleteMRemoteTrainingSystem ~
      getRemoteTrainingSystemRevisions ~
      createRemoteTrainingSystemRevision
  }

  override def convertCAMSResultsToResources(first: Option[HyperReference] = None,
                                             next: Option[HyperReference] = None,
                                             limit: Option[Int],
                                             totalRows: Option[Int],
                                             results: Vector[Asset]): RemoteTrainingSystemResources = {

    val convertedResult: Vector[(Option[RemoteTrainingSystemResource], Option[Warning])] = results.map { asset =>
      val assetJson = asset.toJson
      // here we need to filter out the corrupted data, this should never happened
      Try(convertCAMSResource(assetJson, assetType)) match {
        case Success(j) => (Some(j), None)
        case Failure(e) => (None, Some(handleCamsConvertToWmlError(asset, e)))
      }
    }
    val resources = convertedResult.flatMap(_._1)
    val system = getSystemForResources(convertedResult.flatMap(_._2))
    RemoteTrainingSystemResources(first, next, limit, totalRows, resources, system)
  }

  override def convertEntityJsonToResource(entityJs: JsValue, metadata: Metadata): RemoteTrainingSystemResource = {
    val entity = entityJs.convertTo[RemoteTrainingSystemEntity]
    RemoteTrainingSystemResource(metadata, entity)
  }

  override def convertEntityRequestToJson(entityRequest: RemoteTrainingSystemEntityRequest): JsValue = {
    entityRequest.toJson
  }

  override def addFieldsToCAMSEntity(entity: RemoteTrainingSystemEntityRequest): JsValue = {
    entity.toJson
  }

  override def removeFieldsFromCAMSEntity(entityJs: JsValue): JsValue = {
    removeMetadataFieldsFromEntity(entityJs)
  }

  override def getCAMSMetadata(entity: RemoteTrainingSystemEntityRequest): AssetMetadata = {
    convertToCAMSMetadata(entity.name,
      entity.description,
      assetType = assetType,
      entity.tags.map(_.toVector),
      entity.spaceId,
      entity.projectId)
  }

  override def validateEntity(entity: RemoteTrainingSystemEntityRequest)
                             (implicit callContext: CallContext): Future[RemoteTrainingSystemEntityRequest] = {
    logger.trace(s"remote_training_system entity before validation ${logPrint(entity.toJson)}")
    val trimName = unsafeCharactersValidator(entity.name, "name", true)
    val trimDescription = unsafeCharactersValidator(entity.description, "description")
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    val validatedOrganization = validateOrganization(entity.organization)
    val validatedAllowedIdentities = validateAllowedIdentities(entity.allowedIdentities)
    val validatedRemoteAdmin = validateRemoteAdmin(entity.remoteAdmin)

    val entityCopy = entity.copy(name = trimName, description = trimDescription, spaceId = trimSpaceId, projectId = trimProjectId,
      organization = validatedOrganization, allowedIdentities = validatedAllowedIdentities, remoteAdmin = validatedRemoteAdmin)
    logger.trace(s"remote_training_system entity after validation ${logPrint(entityCopy.toJson)}")
    Future.successful(entityCopy)
  }

  override def validatePatchEntity(entity: List[PatchPayloadElement])
                                  (implicit callContext: CallContext): Future[JsValue] = {
    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
    val validateMap: Map[String, PatchPayloadElement => PatchPayloadElement] = Map(
      "/organization" -> {
        (patch: PatchPayloadElement) => {
          val org = validateOrganization(patch.value.map(_.convertTo[RemoteOrganization]))
          patch.copy(value = org.map(_.toJson))
        }
      },
      "/allowed_identities" -> {
        (patch: PatchPayloadElement) => {
          if (patch.op.equals("remove")) {
            removeError("allowed_identities")
          } else {
            val allowedIdentities = validateAllowedIdentities(patch.value.map(_.convertTo[Vector[AllowedIdentity]]).getOrElse(Vector.empty))
            patch.copy(value = Some(allowedIdentities.toJson))
          }

        }
      },
      "/remote_admin" -> {
        (patch: PatchPayloadElement) => {
          val remote_admin = validateRemoteAdmin(patch.value.map(_.convertTo[AllowedIdentity]))
          patch.copy(value = remote_admin.map(_.toJson))
        }
      }
    )
    val patchEntity = entityValidatePatchPayloadByPathName(entity, Some(validateMap))
    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
    Future.successful(patchEntity.toJson)
  }

  override def getMLVersion: String = WML_REMOTE_TRAINING_SYSTEM_ASSET_TYPE_ML_VERSION

  private def getAllRemoteTrainingSystems: Route = resourceAPI.getAllResources

  private def createRemoteTrainingSystem: Route = resourceAPI.createResource

  private def getRemoteTrainingSystem: Route = resourceAPI.getResource

  private def patchRemoteTrainingSystem: Route = resourceAPI.patchResource

  private def deleteMRemoteTrainingSystem: Route = resourceAPI.deleteResource

  private def getRemoteTrainingSystemRevisions: Route = resourceAPI.getAllRevisions

  private def createRemoteTrainingSystemRevision: Route = resourceAPI.createRevision

}
