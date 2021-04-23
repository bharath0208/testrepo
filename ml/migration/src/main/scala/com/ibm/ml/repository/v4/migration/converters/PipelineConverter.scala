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

import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecificationResources
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecifications
import com.ibm.analytics.wml.api.v4.common.Compute
import com.ibm.analytics.wml.api.v4ga.common.{ResourceRef, SoftwareSpecRef}
import com.ibm.analytics.wml.utils._
import com.ibm.analytics.wml.utils.specs.PipelineJsonFormats._
import com.ibm.analytics.wml.utils.specs._
import com.ibm.ml.repository.v4.migration.models.MigrationResults
import spray.json._

import scala.collection.mutable

trait PipelineConverter extends ConversionHelper {

  protected def convertPipelineDoc(pipelineDoc: Option[specs.PipelineDoc],
                                   softwareSpecs: SoftwareSpecifications,
                                   hardwareSpecs: HardwareSpecificationResources,
                                   mappings: Option[Map[String, String]],
                                   results: MigrationResults,
                                   pipelineId: String): (Map[String, JsValue], Seq[String]) = {
    val consoleMessages: mutable.Buffer[String] = mutable.Buffer.empty

    pipelineDoc.map { pd =>
      val convertedPipelineDoc = pd.copy(
        runtimes = pd.runtimes.map(
          _.map { runtime =>
            val (newAppData, consoleMsgs) = convertAppData(runtime.appData, softwareSpecs, hardwareSpecs, runtime.name, runtime.version, pipelineId)
            consoleMessages.appendAll(consoleMsgs)
            runtime.copy(
              appData = newAppData
            )
          }
        ),
        pipelines = pd.pipelines.map(
          pipelineDef => pipelineDef.copy(
            nodes = pipelineDef.nodes.map(node =>
              node.copy(
                parameters = node.parameters.map { nodeParams =>
                  //v3 already translated to v4
                  //V4 pipelines can contain a parameter "training_lib_href", should be migrated to model_definition
                  nodeParams.get(FIELD_TRAINING_LIB_HREF).map(trainingLibRef => {
                    val modelDefId = findResultId(hrefToId(trainingLibRef.asInstanceOf[JsString].value),
                      TRAINING_LIBS_TYPE, results, mappings)
                    import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
                    nodeParams - FIELD_TRAINING_LIB_HREF + (FIELD_MODEL_DEFINITION -> ResourceRef(id = modelDefId).toJson)
                  }).getOrElse(nodeParams)
                }
              )
            )
          )
        )
      )

      //if V4 pipelineNode.parameters contains "compute" or if V3 pipelineNode.parameters contains "compute_configuration_name"/"compute_configuration_nodes",
      // add a runtimeDef with equivalent HardwareSpec to pipelineDoc.runtimes
      //if pipelineDoc.runtimes already contains a runtimeDef with id == pipelineNode.runtimeRef, nothing needs to be done
      val convertRuntimes = convertedPipelineDoc.runtimes.getOrElse(List.empty)

      val newRuntimes = (for {
        pipelineDef <- convertedPipelineDoc.pipelines
        pipelineNode <- pipelineDef.nodes
        parameters <- pipelineNode.parameters
      } yield {
        import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
        //Add V3 compute params to runtimes
        val v3ComputeName = parameters.get(V3_COMPUTE_CONFIG_NAME)
        val v4Compute = parameters.get(FIELD_COMPUTE)

        val compute = (v3ComputeName, v4Compute) match {
          case (Some(cName), _) =>
            val cVersion = parameters.getOrElse(V3_COMPUTE_CONFIG_NODES, 1.toJson)
            Some(Compute(cName.convertTo[String], cVersion.convertTo[Int]))
          case (_, Some(v4Compute)) =>
            Some(v4Compute.convertTo[Compute])
          case _ => None
        }
        createRuntime(pipelineNode.runtimeRef, compute, hardwareSpecs)
      }).flatten

      val finalRuntimes = newRuntimes.foldLeft(convertRuntimes) { (results, runtime) =>
        val rtIndex = results.indexWhere(_.id == runtime.id)
        if (rtIndex != -1) {
          val currentRT = results(rtIndex)
          val appDataDefined = currentRT.appData.isDefined
          val newHardwareSpec = runtime.appData.flatMap(_.wmlData.hardwareSpec)
          if (appDataDefined) {
            val appData = currentRT.appData.get
            val hardwareSpecEmpty: Boolean = appData.wmlData.hardwareSpec.isEmpty
            if (hardwareSpecEmpty) {
              val mergedRes = currentRT.copy(appData =
                Some(appData.copy(wmlData = appData.wmlData.copy(hardwareSpec = newHardwareSpec)))
              )
              results.updated(rtIndex, mergedRes)
            } else results
          } else {
            val mergedRes = currentRT.copy(appData = runtime.appData)
            results.updated(rtIndex, mergedRes)
          }
        } else results.appended(runtime)
      }

      val finalPipelineDoc = convertedPipelineDoc.copy(runtimes = Some(finalRuntimes))
      (jsonMap(FIELD_DOCUMENT, finalPipelineDoc.toJson), consoleMessages.toSeq)
    }.getOrElse(Map.empty, Seq.empty)
  }

  protected def convertAppData(appData: Option[AppData],
                               softwareSpecs: SoftwareSpecifications,
                               hardwareSpecs: HardwareSpecificationResources,
                               runtimeName: String,
                               runtimeVersion: Option[String],
                               pipelineId: String): (Option[AppData], Seq[String]) = {

    val (newAppData, consoleMsgs) = appData.map { appData =>
      val (wmlData, consoleMsgs) = convertWMLData(
        appData.wmlData,
        softwareSpecs,
        hardwareSpecs,
        runtimeName,
        runtimeVersion,
        pipelineId
      )
      (appData.copy(wmlData = wmlData), consoleMsgs)
    }.getOrElse {
      //for a runtimeDef (like the one below) with "name" and optional "version" fields (and not appData),
      /*"runtimes": [
        {
          "id": "DL",
          "name": "tensorflow",
          "version": "1.15-py3.6"
        }
      ]*/
      val (softwareSpecRef, consoleMsgs) = convertRuntimeNameToSoftwareSpec(runtimeName, runtimeVersion, softwareSpecs, pipelineId)
      import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
      val wmlData = softwareSpecRef.toJson.convertTo[WmlData]
      (AppData(wmlData, None), consoleMsgs)
    }
    (Some(newAppData), consoleMsgs)
  }


  protected def convertWMLData(wmlData: WmlData,
                               softwareSpecs: SoftwareSpecifications,
                               hardwareSpecs: HardwareSpecificationResources,
                               runtimeName: String,
                               runtimeVersion: Option[String],
                               pipelineId: String): (WmlData, Seq[String]) = {
    import com.ibm.analytics.wml.utils.specs.PipelineJsonFormats._
    val wmlDataMap = wmlData.toJson.asJsObject.fields
    (wmlData.runtimeSpecV4, wmlData.runtimeSpec) match {
      case (Some(runtimeSpecV4), _) =>
        val (fields, consoleMsgs) = convertV4RuntimeSpecToHardwareAndSoftwareSpec(runtimeSpecV4, runtimeName, runtimeVersion, softwareSpecs, hardwareSpecs, pipelineId)
        ((wmlDataMap ++ fields).toJson.convertTo[WmlData], consoleMsgs)
      case (None, Some(runtimeSpec)) =>
        val (fields, consoleMsgs) = convertV3RuntimeSpecToHardwareAndSoftwareSpec(runtimeSpec, runtimeName, runtimeVersion, softwareSpecs, hardwareSpecs, pipelineId)
        ((wmlDataMap ++ fields).toJson.convertTo[WmlData], consoleMsgs)
      case _ =>
        (wmlData, Seq.empty)
    }
  }

  private def convertV4RuntimeSpecToHardwareAndSoftwareSpec(runtimeSpec: RuntimeSpecV4,
                                                            runtimeName: String,
                                                            runtimeVersion: Option[String],
                                                            softwareSpecs: SoftwareSpecifications,
                                                            hardwareSpecs: HardwareSpecificationResources,
                                                            pipelineId: String): (Map[String, JsValue], Seq[String]) = {
    val hardwareSpecMap = runtimeSpec.compute.map { compute =>
      val newCompute = Some(Compute(compute.name, compute.nodes.getOrElse(1)))
      convertComputeToHardwareSpecRef(compute = newCompute, hwSpecResources = hardwareSpecs)
    }.getOrElse(Map.empty)

    val (softwareSpecMap, consoleMsgs) = convertRuntimeSpecV4ToSoftwareSpec(runtimeName, runtimeVersion, softwareSpecs, pipelineId)
    (hardwareSpecMap ++ softwareSpecMap, consoleMsgs)
  }

  private def convertV3RuntimeSpecToHardwareAndSoftwareSpec(runtimeSpec: RuntimeSpecV3,
                                                            runtimeName: String,
                                                            runtimeVersion: Option[String],
                                                            softwareSpecs: SoftwareSpecifications,
                                                            hardwareSpecs: HardwareSpecificationResources,
                                                            pipelineId: String): (Map[String, JsValue], Seq[String]) = {

    val (swSpec, consoleMsgs): (Map[String, JsValue], Seq[String]) = runtimeSpec.language.map { language =>
      convertRuntimeSpecV3ToSoftwareSpec(language, runtimeName, runtimeVersion, softwareSpecs, pipelineId)
    }.getOrElse(Map.empty, Seq.empty)
    val hwSpec: Map[String, JsValue] = runtimeSpec.compute.map { compute =>
      val newCompute = Some(Compute(compute.name, compute.nodes.getOrElse(1)))
      convertComputeToHardwareSpecRef(compute = newCompute, hwSpecResources = hardwareSpecs)
    }.getOrElse(Map.empty)
    (swSpec ++ hwSpec, consoleMsgs)

  }


  protected def convertRuntimeSpecV3ToSoftwareSpec(runtimeLanguage: Versioned,
                                                   runtimeName: String,
                                                   runtimeVersion: Option[String],
                                                   softwareSpecs: SoftwareSpecifications,
                                                   pipelineId: String): (Map[String, JsValue], Seq[String]) = {

    val languageName = if (runtimeLanguage.name.equals("python")) "py" else runtimeLanguage.name
    val versionLanguageStr = runtimeVersion.map(version => s"$version-$languageName${runtimeLanguage.version}").getOrElse(s"$languageName${runtimeLanguage.version}")
    //runtimeName="tensorflow", runtimeVersion="1.15", runtimeLanguage[name="python", version="3.6"]
    //yields versionLanguageStr="1.15-py3.6"
    convertRuntimeNameToSoftwareSpec(runtimeName, Some(versionLanguageStr), softwareSpecs, pipelineId)
  }


  private def createRuntime(id: Option[String], compute: Option[Compute], hardwareSpecs: HardwareSpecificationResources): Option[RuntimeDef] = {
    import com.ibm.analytics.wml.utils.specs.PipelineJsonFormats._

    for {
      js <- convertComputeToHardwareSpecRef(compute, hardwareSpecs).get(FIELD_HARDWARE_SPEC)
      refId <- id
    } yield {
      val hwSpecRef = js.convertTo[HardwareSpec]
      RuntimeDef(id = refId,
        name = refId,
        appData = Some(AppData(WmlData(None, None, None, None, Some(hwSpecRef), None), None))
      )
    }
  }

  protected def convertRuntimeSpecV4ToSoftwareSpec(runtimeName: String,
                                                   runtimeVersion: Option[String],
                                                   softwareSpecs: SoftwareSpecifications,
                                                   pipelineId: String): (Map[String, JsValue], Seq[String]) = {
    runtimeName match {
      case AUTO_AI_KB =>
        val KBSoftwareSpec = getBaseSoftwareSpecByName(softwareSpecs, AUTO_AI_KB_SW_SPEC_NAME)
        (KBSoftwareSpec, Seq.empty)
      case AUTO_AI_OBM =>
        val OBMSoftwareSpec = getBaseSoftwareSpecByName(softwareSpecs, AUTO_AI_OBM_SW_SPEC_NAME)
        (OBMSoftwareSpec, Seq.empty)
      case _ =>
        convertRuntimeNameToSoftwareSpec(runtimeName, runtimeVersion, softwareSpecs, pipelineId)
    }
  }


  protected def convertRuntimeNameToSoftwareSpec(runtimeName: String, runtimeVersion: Option[String], swSpecResources: SoftwareSpecifications, pipelineId: String): (Map[String, JsValue], Seq[String]) = {
    val runtimeNameVersion = runtimeVersion.map(version => runtimeName + "_" + version).getOrElse(runtimeName)
    //look for a software_specification with the same name as runtime
    val (newId, consoleMsgs) = getSoftwareSpecFromMap(swSpecResources, runtimeNameVersion)
    if (newId.isDefined) {
      import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
      val result = jsonMap(FIELD_SOFTWARE_SPEC, SoftwareSpecRef(id = newId).toJson)
      (result, consoleMsgs)
    } else {
      val msg = s"Unable to convert the runtime $runtimeNameVersion for pipeline asset $pipelineId to a software specification"
      logger.error(msg)
      val messages = Seq(s"Warning: $msg. If the pipeline training fails, create the asset manually.")
      (Map.empty, consoleMsgs :++ messages)
    }
  }
}
