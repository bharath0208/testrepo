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

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.api.v4ga.training_definitions.TrainingDefinitionEntityRequest
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.utils.{CallContext, InvalidRequestEntityMessage, ServiceException}

import scala.concurrent.{ExecutionContext, Future}

trait TrainingDefinitionsValidator extends ValidateUtils {

  protected def checkHardwareSpecsExistence(entity: TrainingDefinitionEntityRequest): Boolean = {

    // pipeline.hardware_spec.id
    // pipeline.hybrid_pipeline_hardware_specs[].hardware_spec.id
    if (checkHardwareSpecExistInPipelineRef(entity.pipeline)) return true
    // model_definition.hardware_spec.id
    if (checkHardwareSpecExistInModelDefinitionRef(entity.modelDefinition)) return true
    // federated_learning.hardware_spec.id
    if (checkHardwareSpecExistInFederatedLearning(entity.federatedLearning)) return true
    false
  }


  def entityHWSWSpecValidator(entity: TrainingDefinitionEntityRequest,
                              envClient: EnvironmentsClient)
                             (implicit callContext: CallContext, ec: ExecutionContext): Future[TrainingDefinitionEntityRequest] = {

    val hardwareSpecExistent = checkHardwareSpecsExistence(entity)
    val softwareSpecExistent = checkSoftwareSpecExistInModelDefinitionRef(entity.modelDefinition)
    val (hardwareSpecHelperMaps, softwareSpecHelperMaps) = getSWHWSpecHelperMap(hardwareSpecExistent, softwareSpecExistent, envClient)

    (for {
      (hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap) <- hardwareSpecHelperMaps
      (softwareSpecNameToIdMap, softwareSpecIdToTypeMap) <- softwareSpecHelperMaps
    } yield {
      val pipelineCopy = validatePipelineRef(entity.pipeline, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      val modelDefinitionCopy = validateModelDefinitionRef(entity.modelDefinition,
        hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap,
        softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
      val federatedLearningCopy = validateFederatedLearning(entity.federatedLearning,
        hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)

      entity.copy(pipeline = pipelineCopy, modelDefinition = modelDefinitionCopy, federatedLearning = federatedLearningCopy)
    }) recoverWith {
      case t: Throwable =>
        logger.error(s"Validate Software/Hardware Spec error: ${t.getLocalizedMessage}")
        Future.failed(t)
    }
  }

  def trainingFieldValidator(entity: TrainingDefinitionEntityRequest): Unit = {
    val experiment = entity.experiment
    val pipeline = entity.pipeline
    val modelDef = entity.modelDefinition
    val fl = entity.federatedLearning
    (experiment, pipeline, modelDef, fl) match {
      case (Some(_), None, None, None) =>
      case (None, Some(_), None, None) =>
      case (None, None, Some(_), None) =>
      case (None, None, None, Some(_)) =>
      case _ =>
        val msg = "Either 'experiment', 'pipeline', 'model_definition' or 'federated_learning' field needs to be provided"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }
}
