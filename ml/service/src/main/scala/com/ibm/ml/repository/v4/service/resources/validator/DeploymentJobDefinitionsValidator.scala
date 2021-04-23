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
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions._
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.ibm.ml.repository.v4.utils.{CallContext, InvalidRequestEntityMessage, ServiceException}

import scala.concurrent.{ExecutionContext, Future}

trait DeploymentJobDefinitionsValidator extends ValidateUtils {

  protected def checkHardwareSpecsExistence(entity: DeploymentJobDefinitionEntityRequest): Boolean = {
    //hardware_spec.id
    entity.hardwareSpec.map(_ => {
      return true
    })
    //hybrid_pipeline_hardware_specs[].hardware_spec.id
    entity.hybridPipelineHardwareSpecs.map(hybrid => {
      hybrid.map(_.hardwareSpec).map(_ => {
        return true
      })
    })
    false
  }

  def entityHWSpecValidator(entity: DeploymentJobDefinitionEntityRequest,
                            envClient: EnvironmentsClient)
                           (implicit callContext: CallContext, ec: ExecutionContext): Future[DeploymentJobDefinitionEntityRequest] = {

    val hardwareSpecExistent = checkHardwareSpecsExistence(entity)
    val hardwareSpecHelperMaps = getHWSpecHelperMap(hardwareSpecExistent, envClient)
    (for {
      (hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap) <- hardwareSpecHelperMaps
    } yield {
      val hardwareSpecRefCopy = entity.hardwareSpec.map(hdSpec => {
        validateHardwareSpecs(hdSpec, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      })
      val hybridPipelineHardwareSpecCopy = validateHybridPipelineHardwareSpecs(entity.hybridPipelineHardwareSpecs,
        hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      entity.copy(hardwareSpec = hardwareSpecRefCopy, hybridPipelineHardwareSpecs = hybridPipelineHardwareSpecCopy)
    }) recoverWith {
      case t: Throwable =>
        ExceptionLogger.log(Some("Validate Software/Hardware Spec error"), t, None)
        Future.failed(t)
    }
  }

  def entityScoringValidator(scoring: Option[JobScoring]): Unit = {
    scoring.foreach(s => {
      /* This can not be done here as it depends on the runtime
       * and in functions don't need to set anything.
      // https://github.ibm.com/NGP-TWC/ml-planning/issues/17337
      def score(ref: Option[Vector[AnyRef]]): Int = if (ref.isDefined && ref.get.nonEmpty) 1 else 0
      val defined = score(s.inputData) + score(s.inputDataReferences) + score(s.evaluations)
      // if (!(s.inputData.isEmpty ^ s.inputDataReferences.isEmpty ^ s.evaluations.isEmpty)) { // ^ == XOR
      if (defined != 1) {
        val msg = "Either input_data, input_data_references or evaluations in the scoring has to be provided."
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }
      emptyArrayValidator("input_data in the scoring field", s.inputData)
      emptyArrayValidator("input_data_references in the scoring field", s.inputDataReferences)
      emptyArrayValidator("evaluations", s.evaluations)
       */
    })
  }

  def doInputDataValidator(inputData: DOInputData): Unit = {
    // content case
    if (inputData.content.isDefined) {
      stringFieldValidator(inputData.content, "decision_optimization.input_data[].content")
      if (inputData.values.isDefined || inputData.fields.isDefined) {
        val msg = "'values' and 'fields' fields are mutually exclusive with 'content' field"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }

    } else if (inputData.values.isDefined || inputData.fields.isDefined) {
      if (inputData.values.isEmpty) {
        val msg = "'decision_optimization.input_data[].values' is required when 'decision_optimization.input_data[].fields' is provided"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }

      if (inputData.content.isDefined) {
        val msg = "'values' and 'fields' fields are mutually exclusive with 'content' field"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }
    } else {
      val msg = "Either 'values and fields' fields or 'content' field needs to be provided in decision_optimization.input_data[]"
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }

  def entityDecisionOptimizationValidator(decisionOptimization: Option[JobDecisionOptimization]): Unit = {
    decisionOptimization.foreach(d => {
      emptyArrayValidator("input_data in the decision_optimization field", d.inputData)
      d.inputData.map(_.map(input => doInputDataValidator(input)))
      emptyArrayValidator("input_data_references in the decision_optimization field", d.inputDataReferences)
      emptyArrayValidator("output_data in the decision_optimization field", d.inputData)
      emptyArrayValidator("output_data_references in the decision_optimization field", d.inputDataReferences)
    })
  }
}
