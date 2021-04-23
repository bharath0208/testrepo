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
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecJsonFormat._
import com.ibm.analytics.environments.api.v2.software_spec.{PackageExtensionEntityRequest, PipZip}
import com.ibm.analytics.wml.api.v4.libraries.LibraryJsonFormat._
import com.ibm.analytics.wml.api.v4.libraries.LibraryResource
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.converters.ConversionHelper
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds}
import com.ibm.ml.repository.v4.utils._

import scala.concurrent.Future
import scala.util.Try

case class LibraryMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[LibraryResource, PackageExtensionEntityRequest](mj) with ConversionHelper {

  override def oldAssetType: String = LIBRARIES_TYPE

  override def newAssetType: String = PACKAGE_EXTENSIONS_TYPE

  override protected def getOldAssetAction(id: String, doc: MigrationDoc): Future[(LibraryResource, Option[String], Seq[String])] = {
    for {
      resource <- mj.sc.v4BetaRepositoryClient.library.get(identity, id, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
    } yield {
      (resource, None, Seq.empty)
    }
  }

  override protected def getDependenciesAction(oldResource: LibraryResource,
                                               doc: MigrationDoc): Future[MigrationIds] =
    Future.successful(MigrationIds())

  override protected def convertToRequestAction(oldResource: LibraryResource,
                                                dependencies: MigrationIds,
                                                doc: MigrationDoc):
  Future[(PackageExtensionEntityRequest, Seq[String])] = {
    val entity = oldResource.entity
    val newRequest = PackageExtensionEntityRequest(entity.name, entity.description, Some(PipZip))
    Future.successful(newRequest, Seq.empty)
  }

  override protected def createNewAssetAction(request: PackageExtensionEntityRequest, oldResource: LibraryResource, doc: MigrationDoc): Future[String] = {

    val container = Some(getContainer(doc.newSpaceId, doc.newProjectId))
    for {
      resource <- mj.sc.environmentsClient.createPackageExtension(identity, request, container)
      source <- mj.sc.v4BetaRepositoryClient.library.download(identity, oldResource.metadata.id, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
      url <- Future.fromTry(Try(resource.entity.package_extension.get.href.get).recoverWith {
        t: Throwable =>
          val msg = "Failed to read package extension upload url"
          throw ServiceException(StatusCodes.BadRequest, FailedToMigrateDependencies(msg))
      })
      id <- Future.fromTry(Try(resource.metadata.assetId))
      _ <- mj.sc.environmentsClient.uploadPackageExtensionContentWithStream(identity, url, id, source)
      _ <- mj.sc.environmentsClient.uploadPackageExtensionComplete(identity, id, container)
    } yield {
      id
    }
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    val container = Some(getContainer(doc.newSpaceId, doc.newProjectId))
    mj.sc.environmentsClient.getPackageExtensionResource(identity, newId, container)
  }
}
