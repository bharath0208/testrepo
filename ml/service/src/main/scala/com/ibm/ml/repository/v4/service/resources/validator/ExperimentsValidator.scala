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

import com.ibm.analytics.wml.api.v4ga.experiments.TrainingReference
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.utils.CallContext

import scala.concurrent.{ExecutionContext, Future}

trait ExperimentsValidator extends ValidateUtils {
  protected def checkTrainingReferencesSoftwareSpecsExistence(trainingReferences: Seq[TrainingReference]): Boolean = {
    trainingReferences.foreach(tr => {
      // * training_references[].model_definition.software_spec.id
      if (checkSoftwareSpecExistInModelDefinitionRef(tr.modelDefinition)) return true
    })
    false
  }

  protected def checkTrainingReferencesHardwareSpecsExistence(trainingReferences: Seq[TrainingReference]): Boolean = {
    trainingReferences.foreach(tr => {
      //  * training_references[].pipeline.hardware_spec.id
      //  * training_references[].pipeline.hybrid_pipeline_hardware_specs[].hardware_spec.id
      if (checkHardwareSpecExistInPipelineRef(tr.pipeline)) return true
      // * training_references[].model_definition.hardware_spec.id
      if (checkHardwareSpecExistInModelDefinitionRef(tr.modelDefinition)) return true
    })
    false
  }

  def entityTrainingReferencesValidator(trainingReferences: Option[Seq[TrainingReference]],
                                        envClient: EnvironmentsClient)
                                       (implicit callContext: CallContext, ec: ExecutionContext): Future[Option[Seq[TrainingReference]]] = {
    if (trainingReferences.isEmpty) {
      Future.successful(None)
    } else if (trainingReferences.isDefined && trainingReferences.get.isEmpty) {
      emptyArrayError("training_references field")
    } else { // trainingReference is defined and has length > 0
      val hardwareSpecExistent = checkTrainingReferencesHardwareSpecsExistence(trainingReferences.get)
      val softwareSpecExistent = checkTrainingReferencesSoftwareSpecsExistence(trainingReferences.get)
      val (hardwareSpecHelperMaps, softwareSpecHelperMaps) = getSWHWSpecHelperMap(hardwareSpecExistent, softwareSpecExistent, envClient)
      (for {
        (hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap) <- hardwareSpecHelperMaps
        (softwareSpecNameToIdMap, softwareSpecIdToTypeMap) <- softwareSpecHelperMaps
      } yield {
        trainingReferences.map(trs => {
          trs.map(tr => {
            val pipelineCopy = validatePipelineRef(tr.pipeline, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
            val modelDefinitionCopy = validateModelDefinitionRef(tr.modelDefinition,
              hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap,
              softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
            tr.copy(pipeline = pipelineCopy, modelDefinition = modelDefinitionCopy)
          })
        })
      }) recoverWith {
        case t: Throwable =>
          logger.error(s"Validate experiment training reference error: ${t.getLocalizedMessage}")
          Future.failed(t)
      }
    }
  }
}
