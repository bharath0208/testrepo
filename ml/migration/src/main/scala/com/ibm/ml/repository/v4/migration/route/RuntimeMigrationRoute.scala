/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.route

import akka.actor.ActorSystem
import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecJsonFormat._
import com.ibm.analytics.environments.api.v2.software_spec.{SoftwareSpecificationDependency, SoftwareSpecificationEntityRequest}
import com.ibm.analytics.wml.api.v4.runtimes.RuntimeJsonFormat._
import com.ibm.analytics.wml.api.v4.runtimes.RuntimeResource
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.converters.ConversionHelper
import com.ibm.ml.repository.v4.migration.job.MigrationJob
import com.ibm.ml.repository.v4.migration.models.{MigrationDoc, MigrationIds, MigrationResults}
import com.ibm.ml.repository.v4.utils.{FailedToMigrateAsset, ServiceException, getContainer}

import scala.concurrent.Future

case class RuntimeMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[RuntimeResource, SoftwareSpecificationEntityRequest](mj) with ConversionHelper {

  override def oldAssetType: String = RUNTIMES_TYPE

  override def newAssetType: String = SOFTWARE_SPECIFICATIONS_TYPE

  override protected def getOldAssetAction(id: String): Future[(RuntimeResource, Option[String], Seq[String])] = {
    for {
      runtime <- mj.sc.v4BetaRepositoryClient.runtimes.get(identity = identity, id = id)
    } yield {
      val (newId, consoleMsgs) = getSoftwareSpecFromMap(mj.softwareSpecs, runtime.entity.name)
      (runtime, newId, consoleMsgs)
    }
  }

  override protected def getDependenciesAction(oldResource: RuntimeResource, doc: MigrationDoc): Future[MigrationIds] = {
    val ids = oldResource.entity.customLibraries.map { customLibs =>
      val customLibsToMigrate = customLibs.map(_.getId())
      MigrationIds(libraryIds = customLibsToMigrate)
    }.getOrElse(MigrationIds())
    Future.successful(ids)
  }

  override protected def convertToRequestAction(oldResource: RuntimeResource, dependencies: MigrationIds, doc: MigrationDoc): Future[(SoftwareSpecificationEntityRequest, Seq[String])] = {
    val packageExtensions = oldResource.entity.customLibraries.map { customLibs =>
      customLibs.map(_.getId()).map { oldId =>
        val packageId = findResultId(
          oldId,
          LIBRARIES_TYPE,
          doc.results.getOrElse(MigrationResults()),
          doc.mapping)
        SoftwareSpecificationDependency(
          guid = Some(packageId),
          href = Some(s"$PACKAGE_EXTENSION_BASE_URI/$packageId"))
      }
    }

    val request = mj.softwareSpecs.resources.find(sSpec => {
      val softwareSpecification = sSpec.entity.software_specification
      val spec = softwareSpecification.softwareConfiguration
      val oldPlatform = oldResource.entity.platform
      softwareSpecification.sType.isDefined &&
        softwareSpecification.sType.get == SOFTWARE_SPECIFICATION_TYPE_BASE &&
        spec.isDefined &&
        spec.get.platform.name.getOrElse("") == oldPlatform.name &&
        spec.get.platform.version.getOrElse("") == oldPlatform.version
    }).map { baseSoftwareSpec =>


      SoftwareSpecificationEntityRequest(
        name = oldResource.entity.name,
        description = oldResource.entity.description,
        baseSoftwareSpecification = SoftwareSpecificationDependency(
          guid = Some(baseSoftwareSpec.metadata.assetId),
          href = Some(s"$SOFTWARE_SPECIFICATIONS_BASE_URI/${baseSoftwareSpec.metadata.assetId}")),
        packageExtensions = packageExtensions
      )
    }.getOrElse {
      val message = s"Base software_specification not found for platform[name: ${oldResource.entity.platform.name}, version: ${oldResource.entity.platform.version}]"
      throw ServiceException(
        StatusCodes.BadRequest,
        FailedToMigrateAsset(oldResource.metadata.id, oldAssetType, message)
      )
    }

    Future.successful(request, Seq.empty)
  }

  override protected def createNewAssetAction(request: SoftwareSpecificationEntityRequest, oldResource: RuntimeResource, doc: MigrationDoc): Future[String] = {
    val container = Some(getContainer(doc.spaceId, doc.projectId))
    for {
      swSpec <- mj.sc.environmentsClient.createSoftwareSpec(identity, request, container)
    } yield swSpec.metadata.assetId
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    val container = Some(getContainer(doc.spaceId, doc.projectId))
    mj.sc.environmentsClient.getSoftwareSpecResource(identity, newId, container)
  }
}
