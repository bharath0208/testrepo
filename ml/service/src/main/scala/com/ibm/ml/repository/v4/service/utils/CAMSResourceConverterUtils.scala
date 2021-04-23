/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.utils

import akka.http.scaladsl.model.Uri
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetAttachmentMetadata, AssetMetadata, Assets}
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}
import com.ibm.analytics.wml.utils.envionment.SoftwareSpecsUtils
import com.ibm.analytics.wml.utils.queries.{PrivateCloud, PublicCloud}
import com.ibm.ml.repository.v4.utils._
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import java.net.URLEncoder
import java.text.SimpleDateFormat
import java.util.{Date, Locale, TimeZone}
import scala.util.{Failure, Success, Try}

trait CAMSResourceConverterUtils extends AssetConstant with StrictLogging {
  // get asset metadata (common for all asset types)
  def convertToWMLMetadata(camsMetadata: AssetMetadata): Metadata = {
    logger.debug(s"Start to convert metadata ${logPrint(camsMetadata.toJson)}")
    val assetId = camsMetadata.assetId.getOrElse("N/A") // this will never happened
    val spaceId = camsMetadata.spaceId
    val projectId = camsMetadata.projectId
    val createdAt = if (camsMetadata.created.getOrElse(0) != 0) {
      new Date(camsMetadata.created.get)
    } else {
      camsMetadata.createdAt match {
        case Some(date) =>
          convertStringToDate(date)
        case None =>
          new Date
      }
    }
    val lastUpdatedAt = camsMetadata.usage.flatMap(_.lastAccessTime).map(new Date(_))
    val rev: Option[String] = camsMetadata.revisionId.map(_.toString)
    val commitInfo = camsMetadata.commitInfo.map { info =>
      CommitInformation(convertStringToDate(info.committedAt.getOrElse("")), info.commitMessage)
    }

    val owner = camsMetadata.ownerId.flatMap(id => Some(id).filterNot(shouldMaskUserId))

    Metadata(
      id = assetId,
      createdAt = createdAt,
      modifiedAt = lastUpdatedAt,
      owner = owner,
      rev = rev,
      name = Some(camsMetadata.name),
      description = getDescription(camsMetadata.description),
      spaceId = spaceId,
      projectId = projectId,
      tags = getTags(camsMetadata.tags),
      commitInfo = commitInfo
    )
  }

  // this function will try 2 format, if mismatch it will return the current time
  // this function use UTC time zone, use this when do don't have long number choice
  def convertStringToDate(str: String): Date = {
    val trimmedDate = str.trim
    logger.debug(s"String to be converted as date: $trimmedDate")

    (for {
      date <- convertStringToDate(trimmedDate, "yyyy-MM-dd'T'HH:mm:ss'Z'")
    } yield {
      date
    }) recoverWith {
      case _ =>
        for {
          date <- convertStringToDate(trimmedDate, "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'")
        } yield {
          date
        }
    } match {
      case Success(date) => date
      case _ => new Date
    }
  }


  // this function use UTC time zone, use this when do don't have long number choice
  def convertStringToDate(s: String, format: String): Try[Date] = {
    Try {
      val dateFormat = new SimpleDateFormat(format)
      dateFormat.setTimeZone(TimeZone.getTimeZone("UTC"))
      dateFormat.parse(s)
    }
  }

  def getDescription(description: Option[String]): Option[String] = {
    description.flatMap(des => {
      if (des.isEmpty) None else Some(des)
    })
  }

  def getTags(tags: Option[Vector[String]]): Option[Vector[String]] = {
    tags.flatMap(tags => {
      if (tags.isEmpty) None else Some(tags)
    })
  }

  // remove name description spaceId projectId and Tags from the entity
  def removeMetadataFieldsFromEntity(entity: JsValue): JsValue = {
    val fields = entity.asJsObject.fields - NAME - DESCRIPTION - SPACE_ID - PROJECT_ID - TAGS
    fields.toJson
  }

  def getPageInfo(resource: Assets, uri: Uri): (Option[HyperReference], Option[Int], Option[HyperReference]) = {
    val first: Option[HyperReference] = Some(HyperReference(getFirstUrl(uri)))
    val limit: Option[Int] = resource.next.flatMap(_.limit)
    val next: Option[HyperReference] = resource.next.flatMap(_.bookmark).map(bookmark => HyperReference(getNextUrl(uri, bookmark)))
    (first, limit, next)
  }

  def getRevisionsPageInfo(results: Vector[Asset], uri: Uri, limit: Option[Int]): (Option[HyperReference], Option[HyperReference]) = {
    val restRows = if (results.isEmpty) None else results.head.metadata.revisionId
    val first: Option[HyperReference] = Some(HyperReference(getFirstUrl(uri)))
    val next = for {
      l <- limit
      t <- restRows
      if t > l
    } yield {
      val startField = t - l
      HyperReference(getNextUrl(uri, startField.toString))
    }
    (first, next)
  }

  //remove start query param ( if exists ) return the url
  def getFirstUrl(uri: Uri): String = {
    val query = removeStartQuery(uri)
    if (query.nonEmpty)
      s"${uri.path.toString}?${query.toString}"
    else
      uri.path.toString()
  }

  def removeStartQuery(uri: Uri): Uri.Query = uri.query().filter(_._1 != "start")

  //remove start query param ( if exists ) return the url
  def getNextUrl(uri: Uri, bookmark: String): String = {
    val query = removeStartQuery(uri)
    if (query.nonEmpty)
      s"${uri.path.toString}?${query.toString}&start=$bookmark"
    else
      s"${uri.path.toString}?start=$bookmark"
  }

  // convert asset metadata (common for all asset types)
  def convertToCAMSMetadata(name: String,
                            description: Option[String],
                            assetType: String,
                            tags: Option[Vector[String]] = None,
                            spaceId: Option[String] = None,
                            projectId: Option[String] = None): AssetMetadata = {

    val originCountry = Try {
      Locale.getDefault.getCountry
    } match {
      case Success(locale) => locale
      case Failure(_) => "US"
    }
    AssetMetadata(
      name = name,
      description = description,
      tags = tags,
      assetType = assetType,
      originCountry = originCountry,
      assetCategory = "USER",
      spaceId = spaceId,
      projectId = projectId)
  }

  def encode(s: String): String = URLEncoder.encode(s, "UTF-8")

  // when asset id is provided. return revision Href
  def v4ResourcesHref(assetName: String,
                      container: Container,
                      start: Option[String] = None,
                      limit: Option[Int] = None,
                      tagValue: Option[String] = None,
                      assetId: Option[String] = None): String = {
    val path = assetId.map(id => s"$API_PREFIX/${assetName}s/$id/revisions")
      .getOrElse(s"$API_PREFIX/${assetName}s")


    val (k, v) = container match {
      case Space(value) => ("space_id", value)
      case Project(value) => ("project_id", value)
    }
    val containerQuery = s"$k=$v&"
    val query = containerQuery +
      start.map(s => s"start=${encode(s)}&").getOrElse("") +
      limit.map(l => s"limit=${encode(l.toString)}&").getOrElse("") +
      tagValue.map(t => s"tag.value=${encode(t)}&").getOrElse("")
    val url = s"$path?$query"
    if (url.takeRight(1) == "&" || url.takeRight(1) == "?") url.dropRight(1) else url
  }

  def getListOpt(list: Seq[String], key: String): Option[Map[String, JsValue]] = {
    import DefaultJsonProtocol._
    if (list.nonEmpty) Some(Map(key -> list.toJson)) else None
  }

  def getDataAssetsIdsFromDataRefs(dataRefs: Vector[DataReference]): Vector[String] = {
    dataRefs.filter(_.dataSourceType.name == DataAssetType.name).flatMap(_.location.get("id")).distinct
  }

  def handleCamsConvertToWmlError(asset: Asset, exception: Throwable): Warning = {
    val id = asset.metadata.assetId
    val message = exception.getLocalizedMessage
    Warning(id, message)
  }

  def getSystemForResources(warnings: Vector[Warning]): Option[SystemDetails] = {
    if (warnings.isEmpty) None else {
      Some(SystemDetails(Some(warnings.+:(Warning(None, message = "Failed to convert the following assets to WML resources")))))
    }
  }

  def addWarningMessages(messages: Vector[String], systemOpt: Option[SystemDetails]): Option[SystemDetails] = {
    val warnings = messages.map(Warning(None, _))
    val newWarnings = systemOpt.flatMap(_.warnings.map(w => w ++ warnings)).getOrElse(warnings)
    Some(SystemDetails(Some(newWarnings)))
  }

  def getSupportedFrameworksUrl: String = {
    if (isPrivateCloud) "https://www.ibm.com/support/knowledgecenter/SSQNUZ_current/wsj/analyze-data/pm_service_supported_frameworks.html"
    else "https://dataplatform.cloud.ibm.com/docs/content/wsj/analyze-data/pm_service_supported_frameworks.html"
  }

  def getSWSpecWarningMessage(assetType: String, id: Option[String], name: Option[String]): String = {
    val NameOrId: String = name.getOrElse(id.getOrElse(""))
    s"Software specification $NameOrId specified for the $assetType is deprecated and will be removed in the future. We recommend you use a supported software specification. See Supported Frameworks $getSupportedFrameworksUrl."
  }

  def getSoftwareSpecWarningMessages(softwareSpecList: Vector[SoftwareSpecRef], assetType: String): Vector[String] = {
    val environment = if (isPublicCloud) PublicCloud else PrivateCloud
    softwareSpecList.flatMap { sw =>
      if (SoftwareSpecsUtils.isSoftwareSpecsDeprecated(sw.id, sw.name, environment))
        Some(getSWSpecWarningMessage(assetType, sw.id, sw.name))
      else None
    }
  }

  def filterContentMetadata(contentFormat: Option[String],
                            name: Option[String],
                            pipelineNodeId: Option[String],
                            deploymentId: Option[String],
                            metadataList: Vector[AssetAttachmentMetadata],
                            defaultContentType: String = CONTENT_FORMAT_DEFAULT_VALUE): Vector[ContentMetadata] = {
    val nameFilter = name.isDefined
    val nodeIdFilter = pipelineNodeId.isDefined
    val deploymentIdFilter = deploymentId.isDefined
    metadataList.map { attachment =>
      val attachmentId = attachment.attachmentId
      val contentFormat = attachment.userData
        .map(_.fields.getOrElse(CONTENT_FORMAT, JsString(defaultContentType)).convertTo[String]).getOrElse(defaultContentType)
      val name = attachment.name
      val pipelineNodeId = attachment.userData
        .flatMap {
          ud => {
            ud.fields.get(PIPELINE_NODE_ID).map(_.convertTo[String])
          }
        }
      val deploymentId = attachment.userData
        .flatMap {
          ud => {
            ud.fields.get(DEPLOYMENT_ID).map(_.convertTo[String])
          }
        }
      val persisted = attachment.isTransferComplete.getOrElse(false)
      ContentMetadata(attachmentId = attachmentId, contentFormat = contentFormat, persisted, name = name, pipeLineNodeId = pipelineNodeId, deploymentId = deploymentId)
    }
      .filter(contentFormat.isEmpty || contentFormat.get == _.contentFormat)
      .filter(!nameFilter || name == _.name)
      .filter(!nodeIdFilter || pipelineNodeId == _.pipeLineNodeId)
      .filter(!deploymentIdFilter || deploymentId == _.deploymentId)
  }
}
