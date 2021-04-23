/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.converters

import java.net.URI

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecificationResources
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecifications
import com.ibm.analytics.wml.api._
import com.ibm.analytics.wml.api.v4.common.{Compute, ResourceRef, Tag, Tags}
import com.ibm.analytics.wml.api.v4ga.common.{HardwareSpecRef, SoftwareSpecRef}
import com.ibm.ml.repository.v4.migration.models.MigrationResults
import com.ibm.ml.repository.v4.migration.utils.MigrationConfig._
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.utils.{FailedToMigrateDependencies, ServiceException}
import com.typesafe.scalalogging.StrictLogging
import spray.json.DefaultJsonProtocol._
import spray.json._

trait ConversionHelper extends MigrationConstant with StrictLogging {


  protected def getNewName(name: Option[String], oldAssetType: String, id: String): Map[String, JsValue] =
    jsonMap(FIELD_NAME, name.getOrElse(generateName(oldAssetType, id)).toJson)


  protected def convertResourceRef(resourceRef: Option[ResourceRef],
                                   resourceType: String,
                                   newFiledName: String,
                                   results: MigrationResults,
                                   mappings: Option[Map[String, String]],
                                   default: Map[String, JsValue] = Map.empty): Map[String, JsValue] = {
    resourceRef.map { rr =>
      val oldId = rr.getId()
      val newId = findResultId(oldId, resourceType, results, mappings)
      import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
      jsonMap(newFiledName, v4ga.common.ResourceRef(id = newId).toJson)
    }.getOrElse(default)
  }


  protected def jsonMap(key: String, value: JsValue): Map[String, JsValue] = Map(key -> value)

  protected def findResultId(oldId: String, oldAssetType: String, results: MigrationResults, mapping: Option[Map[String, String]] = None): String = {
    // mapping take priority
    mapping.foreach { map =>
      val mOpt = map.get(oldId)
      if (mOpt.isDefined) return mOpt.get
    }
    results.skipped.flatMap(_.find(result => result.oldId == oldId && result.oldAssetType == oldAssetType)).foreach { result =>
      return result.newId
    }
    results.successful.find(result => result.oldId == oldId && result.oldAssetType == oldAssetType).getOrElse {
      val msg = s"$oldAssetType $oldId"
      throw ServiceException(StatusCodes.BadRequest, FailedToMigrateDependencies(msg))
    }.newId

  }


  protected def getSoftwareSpecFromMap(softwareSpecs: SoftwareSpecifications, runtimeName: String): (Option[String], Seq[String]) = {
    softwareSpecs.resources.find(_.metadata.name.getOrElse("") == runtimeName) match {
      case Some(swSpec) => (Some(swSpec.metadata.assetId), Seq.empty)
      case _ =>
        //exact match not available, look for best match (defined in configuration)
        runtimeSoftwareSpecMap.get(runtimeName).map(requiredSwSpecName => {
          softwareSpecs.resources.find(_.metadata.name.get == requiredSwSpecName) match {
            case Some(sSpec) =>
              val msg = s"Failed to find an exact match to map runtime $runtimeName to a software specification. Using software spec ${sSpec.metadata.assetId}"
              logger.info(msg)
              val consoleMsgs = Seq(s"Warning: $msg. If the asset fails to deploy, create a software spec manually.")
              (Some(sSpec.metadata.assetId), consoleMsgs) // base software_spec available with `best match` name defined in configuration
            case _ => (None, Seq.empty) // base software_spec with `best match` name defined in configuration NOT available
          }
        }).getOrElse((None, Seq.empty)) //`best match` NOT defined in configuration
    }
  }

  protected def convertTags(tags: Option[Tags]): Map[String, JsValue] = {
    tags.map { tags =>
      val newTags = tags match {
        case Left(tags) =>
          tags.map(_.value)
        case Right(tags) => tags
      }
      jsonMap(FIELD_TAGS, newTags.toJson)

    }.getOrElse(Map.empty)
  }

  protected def convertSeqTags(tags: Option[Seq[Tag]]): Map[String, JsValue] = {
    tags.map { tags =>
      jsonMap(FIELD_TAGS, tags.map(_.value).toJson)
    }.getOrElse(Map.empty)
  }

  protected def addSpaceOrProjectId(spaceId: Option[String], projectId: Option[String]): Map[String, JsValue] = {
    if (spaceId.isDefined && !spaceId.get.trim.isEmpty) {
      jsonMap(FIELD_SPACE_ID, JsString(spaceId.get))
    } else if (projectId.isDefined && !projectId.get.trim.isEmpty) {
      jsonMap(FIELD_PROJECT_ID, JsString(projectId.get))
    } else {
      // this should never happened
      throw new Exception("Either space_id or project_id has to be provided.")
    }
  }

  protected def generateName(assetType: String, assetId: String): String = assetType + assetId

  protected def hrefToId(href: String): String = {
    val uri = new URI(href)
    uri.getPath.split("/").last
  }

  protected def getBaseSoftwareSpecByName(softwareSpecs: SoftwareSpecifications, softwareSpecName: String, overrideException: Option[Throwable] = None): Map[String, JsValue] = {
    softwareSpecs.resources.find(swSpec => swSpec.metadata.name.getOrElse("") == softwareSpecName) match {
      case Some(softwareSpec) =>
        import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
        jsonMap(FIELD_SOFTWARE_SPEC, SoftwareSpecRef(id = Some(softwareSpec.metadata.assetId), name = Some(softwareSpecName)).toJson)
      case _ =>
        overrideException.map(throw _)
        throw ServiceException(StatusCodes.BadRequest,
          FailedToMigrateDependencies(s"Could not find a base software specification match the name '$softwareSpecName'"))
    }
  }


  protected def convertComputeToHardwareSpecRef(compute: Option[Compute],
                                                hwSpecResources: HardwareSpecificationResources): Map[String, JsValue] = {

    compute.map { c =>
      //look for a hardware_specification with the same name as compute
      val hardwareSpecRef = hwSpecResources.resources.find(_.metadata.name.get == c.name) match {
        case Some(hSpec) => HardwareSpecRef(id = Some(hSpec.metadata.assetId))
        case _ =>
          //exact match not available, look for best match (defined in configuration)
          computeHardwareSpecMap.get(c.name).map(requiredHwSpecName => {
            hwSpecResources.resources.find(_.metadata.name.get == requiredHwSpecName) match {
              case Some(hSpec) => HardwareSpecRef(id = Some(hSpec.metadata.assetId)) // hardware_spec available with `best match` name defined in configuration
              case _ =>
                val msg = s"hardware_specification mapping not found for compute ${c.name}"
                throw ServiceException(StatusCodes.BadRequest, FailedToMigrateDependencies(msg))
            }
          }).getOrElse(throw ServiceException(StatusCodes.BadRequest, FailedToMigrateDependencies(s"Unknown compute ${c.name}"))) //`best match` NOT defined in configuration
      }
      import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
      jsonMap(FIELD_HARDWARE_SPEC, hardwareSpecRef.toJson)
    }.getOrElse(Map.empty)
  }
}