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
import com.ibm.analytics.wml.api._
import com.ibm.analytics.wml.api.v4.common.{HyperReference, TrainingLibRef}
import com.ibm.analytics.wml.api.v4.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common.SoftwareSpecRef
import com.ibm.ml.repository.v4.migration.models.MigrationResults
import spray.json.{JsValue, _}

import scala.util.{Failure, Success, Try}

trait ExperimentConverter extends ConversionHelper {

  def convertTrainingReferences(trainingReferences: Option[Seq[v4.experiments.TrainingReference]],
                                hardwareSpecs: HardwareSpecificationResources,
                                results: MigrationResults,
                                mappings: Option[Map[String, String]],
                                oldAssetId: String): (Map[String, JsValue], Seq[String]) = {
    trainingReferences.map { tr =>
      import v4ga.experiments.ExperimentJsonFormat._
      val (newTrainingRefs, consoleMsgs) = {
        val convertResults = tr.map(convertTrainingReference(_, hardwareSpecs, results, mappings, oldAssetId))
        (convertResults.map(_._1), convertResults.flatMap(_._2))
      }
      (jsonMap(FIELD_TRAINING_REFERENCES, newTrainingRefs.toJson), consoleMsgs)
    }.getOrElse(Map.empty, Seq.empty)
  }

  def convertTrainingReference(trainingReference: v4.experiments.TrainingReference,
                               hardwareSpecs: HardwareSpecificationResources,
                               results: MigrationResults,
                               mappings: Option[Map[String, String]],
                               oldAssetId: String): (v4ga.experiments.TrainingReference, Seq[String]) = {

    val trMap = trainingReference.toJson.asJsObject.fields - FIELD_PIPELINE - FIELD_TRAINING_LIB
    // need to migrate FIELD_PIPELINE, FIELD_TRAINING_LIB
    val newPipeLine = convertPipeLineRef(trainingReference.pipeline, hardwareSpecs, results, mappings)
    val (newModelDef, consoleMsgs) = convertTrainingLibRefToModelDef(trainingReference.trainingLib, hardwareSpecs, results, mappings, oldAssetId)
    import v4ga.experiments.ExperimentJsonFormat._
    ((trMap ++ newPipeLine ++ newModelDef).toJson.convertTo[v4ga.experiments.TrainingReference], consoleMsgs)
  }

  protected def convertPipeLineRef(pipelineRef: Option[v4.common.PipelineRef],
                                   hardwareSpecs: HardwareSpecificationResources,
                                   results: MigrationResults,
                                   mappings: Option[Map[String, String]]): Map[String, JsValue] = {
    import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
    pipelineRef.map { pr =>
      val tlMap = pr.toJson.asJsObject.fields - FIELD_REV - FIELD_HREF - FIELD_COMPUTE - FIELD_ID
      // need to migrate FIELD_COMPUTE, FIELD_ID, FIELD_HYBRID_PIPELINE_HARDWARE_SPECS
      val newHardSpec = convertComputeToHardwareSpecRef(pr.compute, hardwareSpecs)
      val newId = jsonMap(FIELD_ID, findResultId(pr.getId(), PIPELINES_TYPE, results, mappings).toJson)
      val newPipeLineRef = (tlMap ++ newHardSpec ++ newId).toJson
      jsonMap(FIELD_PIPELINE, newPipeLineRef)
    }.getOrElse(Map.empty)
  }

  protected def convertTrainingLibRefToModelDef(trainingLib: Option[TrainingLibRef],
                                                hardwareSpecs: HardwareSpecificationResources,
                                                results: MigrationResults,
                                                mappings: Option[Map[String, String]],
                                                oldAssetId: String): (Map[String, JsValue], Seq[String]) = {
    import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
    trainingLib.map { tl =>
      val tlMap = tl.toJson.asJsObject.fields - FIELD_REV - FIELD_HREF - FIELD_COMPUTE - FIELD_RUNTIME - FIELD_ID
      // need to migrate FIELD_COMPUTE, FIELD_RUNTIME, FIELD_ID
      val (newSoftSpec: Map[String, JsValue], consoleMsgs: Seq[String]) = Try {
        convertRumtimeRefToSoftwareSpecRef(tl.runtime, results, mappings)
      } match {
        case Success(value) => (value, Seq.empty)
        case Failure(exception) =>
          val msg = s"Unable to convert the runtime for experiment asset $oldAssetId due to ${exception.getMessage} to a software specification"
          logger.error(msg)
          val consoleMsgs = Seq(s"Warning: $msg.  If the training fails for this experiment, create the asset manually.")
          (Map.empty, consoleMsgs)
      }
      val newHardSpec = convertComputeToHardwareSpecRef(tl.compute, hardwareSpecs)
      val newId = jsonMap(FIELD_ID, findResultId(tl.getId(), TRAINING_LIBS_TYPE, results, mappings).toJson)

      val newModelDefinitionRef = (tlMap ++ newSoftSpec ++ newHardSpec ++ newId).toJson
      (jsonMap(FIELD_MODEL_DEFINITION, newModelDefinitionRef), consoleMsgs)
    }.getOrElse(Map.empty, Seq.empty)
  }

  protected def convertRumtimeRefToSoftwareSpecRef(runtimeRef: Option[HyperReference],
                                                   results: MigrationResults,
                                                   mappings: Option[Map[String, String]]): Map[String, JsValue] = {
    runtimeRef.map { rtr =>
      val runtimeId = hrefToId(rtr.href)
      val softwareSpecId = findResultId(runtimeId, RUNTIMES_TYPE, results, mappings)
      import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
      jsonMap(FIELD_SOFTWARE_SPEC, SoftwareSpecRef(id = Some(softwareSpecId)).toJson)
    }.getOrElse(Map.empty)
  }


}
