/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources.validator

import com.ibm.analytics.wml.api.v4ga.common.{HardwareSpecRef, SoftwareSpecRef}
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.specs.PipelineDoc
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.service.utils.baseOrDerivedType.baseOrDerivedType
import com.ibm.ml.repository.v4.utils.CallContext

import scala.collection.immutable.Map
import scala.concurrent.{ExecutionContext, Future}

trait PipelinesValidator extends ValidateUtils {

  private def checkDocumentHwExistence(document: PipelineDoc): Boolean = {
    document.runtimes.map(
      _.flatMap(_.appData).map(_.wmlData).map(wmlData => {
        if (wmlData.hardwareSpec.isDefined) return true
      })
    )
    document.appData.map(_.wmlData).foreach(wmlData => {
      if (wmlData.hardwareSpec.isDefined) return true
    })
    false
  }

  private def checkDocumentSwExistence(document: PipelineDoc): Boolean = {
    document.runtimes.map(
      _.flatMap(_.appData).map(_.wmlData).map(wmlData => {
        if (wmlData.softwareSpec.isDefined) return true
      })
    )
    document.appData.map(_.wmlData).foreach(wmlData => {
      if (wmlData.softwareSpec.isDefined) return true
    })
    false
  }

  def entityPipelineDocumentValidator(document: Option[PipelineDoc],
                                      envClient: EnvironmentsClient)
                                     (implicit callContext: CallContext, ec: ExecutionContext): Future[Option[PipelineDoc]] = {
    if (document.isEmpty) {
      Future.successful(None)
    } else {
      val hwSpecsExistence = checkDocumentHwExistence(document.get)
      val swSpecsExistence = checkDocumentSwExistence(document.get)
      val hardwareSpecHelperMaps: Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] =
        if (hwSpecsExistence)
          getHardwareSpecHelperMaps(envClient)
        else Future.successful((Map(), Map()))
      val softwareSpecHelperMaps: Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] =
        if (swSpecsExistence)
          getSoftwareSpecHelperMaps(envClient)
        else Future.successful((Map(), Map()))
      (for {
        (hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap) <- hardwareSpecHelperMaps
        (softwareSpecNameToIdMap, softwareSpecIdToTypeMap) <- softwareSpecHelperMaps
      } yield {
        document.map(doc => {
          val runtimesCopy = doc.runtimes.map(runtimes => {
            runtimes.map(runtime => {
              val appDataCopy = runtime.appData.map(appData => {
                val hdwareSpecCopy: Option[HardwareSpecRef] = appData.wmlData.hardwareSpec.map(hdSpec => {
                  validateHardwareSpecs(HardwareSpecRef(hdSpec), hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
                })
                val sfwareSpecCopy: Option[SoftwareSpecRef] = appData.wmlData.softwareSpec.map(sfSpec => {
                  validateSoftwareSpecs(SoftwareSpecRef(sfSpec), softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
                })
                val wmlDataCopy = appData.wmlData.copy(softwareSpec = toSoftwareSpec(sfwareSpecCopy), hardwareSpec = toHardwareSpec(hdwareSpecCopy))
                appData.copy(wmlData = wmlDataCopy)
              })
              runtime.copy(appData = appDataCopy)
            })
          })
          val appDataCopy = doc.appData.map(appData => {
            val hdwareSpecCopy: Option[HardwareSpecRef] = appData.wmlData.hardwareSpec.map(hdSpec => {
              validateHardwareSpecs(HardwareSpecRef(hdSpec), hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
            })
            val sfwareSpecCopy: Option[SoftwareSpecRef] = appData.wmlData.softwareSpec.map(sfSpec => {
              validateSoftwareSpecs(SoftwareSpecRef(sfSpec), softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
            })
            val wmlDataCopy = appData.wmlData.copy(softwareSpec = toSoftwareSpec(sfwareSpecCopy), hardwareSpec = toHardwareSpec(hdwareSpecCopy))
            appData.copy(wmlData = wmlDataCopy)
          })
          doc.copy(runtimes = runtimesCopy, appData = appDataCopy)
        })
      }) recoverWith {
        case t: Throwable =>
          logger.error(s"Validate experiment training reference error: ${t.getLocalizedMessage}")
          Future.failed(t)
      }
    }
  }
}
