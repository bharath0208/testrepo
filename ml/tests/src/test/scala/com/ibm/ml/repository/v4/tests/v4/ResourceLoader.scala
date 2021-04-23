/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.v4

import java.util.UUID

import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.util.{Failure, Success, Try}
import scala.language.postfixOps


object ResourceLoader extends StrictLogging {

  def loadFromFile(fileName: String,
                             name: Option[String] = None,
                             spaceId: Option[String] = None,
                             projectId: Option[String] = None,
                             baseSwSpecId: Option[String] = None,
                             swSpecId: Option[String] = None,
                             hwSpecId: Option[String] = None,
                             swSpecName: Option[String] = None,
                             hwSpecName: Option[String] = None,
                             modelVersion: Option[String] = None,
                             modelDefinitionId: Option[String] = None,
                             platformName: Option[String] = None,
                             pipelineId: Option[String] = None,
                             pipelineHref: Option[String] = None,
                             experimentId: Option[String] = None,
                             dataAssetId: Option[String] = None,
                             libraryId: Option[String] = None,
                             libraryHref: Option[String] = None,
                             runtimeHref: Option[String] = None,
                             trainingLibHref: Option[String] = None,
                             swSpecHref: Option[String] = None,
                             resourceCrn: Option[String] = None): JsValue = {
    Try {
      val fileRes = getClass.getClassLoader.getResourceAsStream(fileName)
      val bArray = LazyList.continually(fileRes.read).takeWhile(-1 !=).map(_.toByte).toArray
      var updatable = new String(bArray, "UTF-8")
      spaceId.foreach(s => {
        updatable = updatable.replaceAll(">>>CONTAINER_NAME<<<", "space")
        updatable = updatable.replaceAll(">>>CONTAINER_TYPE<<<", "space_id")
        updatable = updatable.replaceAll(">>>CONTAINER_ID<<<", s)
      })
      projectId.foreach(s => {
        updatable = updatable.replaceAll(">>>CONTAINER_NAME<<<", "project")
        updatable = updatable.replaceAll(">>>CONTAINER_TYPE<<<", "project_id")
        updatable = updatable.replaceAll(">>>CONTAINER_ID<<<", s)
      })
      updatable = name.map(updatable.replaceAll(">>>ASSET_NAME<<<", _)).getOrElse(updatable)
      updatable = baseSwSpecId.map(updatable.replaceAll(">>>BASE_SW_SPEC_ID<<<", _)).getOrElse(updatable)
      updatable = swSpecId.map(updatable.replaceAll(">>>SW_SPEC_ID<<<", _)).getOrElse(updatable)
      updatable = hwSpecId.map(updatable.replaceAll(">>>HW_SPEC_ID<<<", _)).getOrElse(updatable)
      updatable = swSpecName.map(updatable.replaceAll(">>>SW_SPEC_NAME<<<", _)).getOrElse(updatable)
      updatable = hwSpecName.map(updatable.replaceAll(">>>HW_SPEC_NAME<<<", _)).getOrElse(updatable)
      updatable = modelVersion.map(updatable.replaceAll(">>>MODEL_VERSION<<<", _)).getOrElse(updatable)
      updatable = platformName.map(updatable.replaceAll(">>>PLATFORM_NAME<<<", _)).getOrElse(updatable)
      updatable = modelDefinitionId.map(updatable.replaceAll(">>>MODEL_DEF_ID<<<", _)).getOrElse(updatable)
      updatable = pipelineId.map(updatable.replaceAll(">>>PIPELINE_ID<<<", _)).getOrElse(updatable)
      updatable = pipelineHref.map(updatable.replaceAll(">>>PIPELINE_HREF<<<", _)).getOrElse(updatable)
      updatable = experimentId.map(updatable.replaceAll(">>>EXPERIMENT_ID<<<", _)).getOrElse(updatable)
      updatable = dataAssetId.map(updatable.replaceAll(">>>DATA_ASSET_ID<<<", _)).getOrElse(updatable)
      updatable = libraryId.map(updatable.replaceAll(">>>LIBRARY_ID<<<", _)).getOrElse(updatable)
      updatable = libraryHref.map(updatable.replaceAll(">>>LIBRARY_HREF<<<", _)).getOrElse(updatable)
      updatable = runtimeHref.map(updatable.replaceAll(">>>RUNTIME_HREF<<<", _)).getOrElse(updatable)
      updatable = trainingLibHref.map(updatable.replaceAll(">>>TRAINING_LIB_HREF<<<", _)).getOrElse(updatable)
      updatable = swSpecHref.map(updatable.replaceAll(">>>SW_SPEC_HREF<<<", _)).getOrElse(updatable)
      updatable = resourceCrn.map(updatable.replaceAll(">>>RESOURCE_CRN<<<", _)).getOrElse(updatable)
      updatable = resourceCrn.map { crn =>
        val guid = crn.split(":")(7)
        updatable.replaceAll(">>>RESOURCE_CRN_GUID<<<", guid)
      }.getOrElse(updatable)
      updatable = updatable.replaceAll(">>>UUID<<<", UUID.randomUUID().toString)

      if (updatable.contains(">>>") && updatable.contains("<<<"))
        throw new Exception(s"File $fileName not completely expanded: $updatable")

      updatable.parseJson
    } match {
      case Success(result) => result
      case Failure(exception) =>
        logger.error(exception.getMessage)
        throw exception
    }
  }

}
