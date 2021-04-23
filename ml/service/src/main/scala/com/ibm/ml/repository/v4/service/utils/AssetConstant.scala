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

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.utils.assets.v4.AssetTypes
import com.ibm.ml.repository.v4.utils.cams.GlobalAssetTypesCreator
import com.ibm.ml.repository.v4.utils.{InternalErrorExceptionMessage, ServiceException}

import scala.util.{Failure, Success}

trait AssetConstant {
  private def getName(assetType: String): String = {
    if (!assetType.startsWith("wml_"))
      throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(s"Invalid asset type $assetType")) // we should never get here
    assetType.substring(4) // AssetTypes.ML_ASSET_TYPE_PREFIX.length)
  }

  private def getLatestVersion(assetType: String): String = {
    AssetTypes.getAssetPath(assetType, GlobalAssetTypesCreator.DEFAULT_USE_VERSION) match {
      case Success((_, version)) => version
      case Failure(exception) => throw exception // we should never get here
    }
  }

  // repo asset name and type
  val WML_PIPELINE_ASSET_TYPE: String = AssetTypes.ML_PIPELINE
  val WML_PIPELINE_ASSET_NAME: String = getName(WML_PIPELINE_ASSET_TYPE)

  val WML_MODEL_ASSET_TYPE: String = AssetTypes.ML_MODEL
  val WML_MODEL_ASSET_NAME: String = getName(WML_MODEL_ASSET_TYPE)

  val WML_EXPERIMENT_ASSET_TYPE: String = AssetTypes.ML_EXPERIMENT
  val WML_EXPERIMENT_ASSET_NAME: String = getName(WML_EXPERIMENT_ASSET_TYPE)

  val WML_FUNCTION_ASSET_TYPE: String = AssetTypes.ML_FUNCTION
  val WML_FUNCTION_ASSET_NAME: String = getName(WML_FUNCTION_ASSET_TYPE)
  val WML_FUNCTION_CONTENT_API_NAME = "code"

  val WML_MODEL_DEFINITION_ASSET_TYPE: String = AssetTypes.ML_MODEL_DEFINITION
  val WML_MODEL_DEFINITION_ASSET_NAME: String = getName(WML_MODEL_DEFINITION_ASSET_TYPE)
  val WML_MODEL_DEFINITION_CONTENT_API_NAME = "model"

  val WML_REMOTE_TRAINING_SYSTEM_ASSET_TYPE: String = AssetTypes.ML_REMOTE_TRAINING_SYSTEM
  val WML_REMOTE_TRAINING_SYSTEM_ASSET_NAME: String = getName(WML_REMOTE_TRAINING_SYSTEM_ASSET_TYPE)

  val WML_TRAINING_DEFINITION_ASSET_TYPE: String = AssetTypes.ML_TRAINING_DEFINITION
  val WML_TRAINING_DEFINITION_ASSET_NAME: String = getName(WML_TRAINING_DEFINITION_ASSET_TYPE)

  val WML_DEPLOYMENT_JOB_DEFINITION_ASSET_TYPE: String = AssetTypes.ML_DEPLOYMENT_JOB_DEFINITION
  val WML_DEPLOYMENT_JOB_DEFINITION_ASSET_NAME: String = getName(WML_DEPLOYMENT_JOB_DEFINITION_ASSET_TYPE)

  val WML_PIPELINE_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_PIPELINE_ASSET_TYPE)
  val WML_MODEL_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_MODEL_ASSET_TYPE)
  val WML_EXPERIMENT_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_EXPERIMENT_ASSET_TYPE)
  val WML_FUNCTION_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_FUNCTION_ASSET_TYPE)
  val WML_MODEL_DEFINITION_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_MODEL_DEFINITION_ASSET_TYPE)
  val WML_REMOTE_TRAINING_SYSTEM_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_REMOTE_TRAINING_SYSTEM_ASSET_TYPE)
  val WML_TRAINING_DEFINITION_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_TRAINING_DEFINITION_ASSET_TYPE)
  val WML_DEPLOYMENT_JOB_DEFINITION_ASSET_TYPE_ML_VERSION: String = getLatestVersion(WML_DEPLOYMENT_JOB_DEFINITION_ASSET_TYPE)

  // other asset type
  val SOFTWARE_SPECIFICATION_ASSET_TYPE = "software_specification"
  val HARDWARE_SPECIFICATION_ASSET_TYPE = "hardware_specification"
  val PACKAGE_EXTENSIONS_ASSET_TYPE = "package_extension"

  // const for field name
  val METADATA = "metadata"
  val CONTENT_FORMAT = "content_format"
  val CONTENT_FORMAT_DEFAULT_VALUE = "native"
  val CONTENT_LOCATION = "content_location"
  val CONTENT_IMPORT_STATE = "content_import_state"
  val PIPELINE_NODE_ID = "pipeline_node_id"
  val DEPLOYMENT_ID = "deployment_id"
  val HARDWARE_SPEC_DEPENDENCIES = "hardware_spec_dependencies"
  val SOFTWARE_SPEC_DEPENDENCIES = "software_spec_dependencies"
  val MODEL_DEFINITION_DEPENDENCIES = "model_definition_dependencies"
  val PIPELINE_DEPENDENCIES = "pipeline_dependencies"
  val DATA_ASSET_DEPENDENCIES = "data_asset_dependencies"
  val NAME = "name"
  val DESCRIPTION = "description"
  val SPACE_ID = "space_id"
  val PROJECT_ID = "project_id"
  val TAGS = "tags"
  val CONTENT_STATUS = "content_status"
  val CONTENT_STATUS_STATE = "state"
  val CONTENT_STATUS_PATH = s"/$CONTENT_STATUS"
  val DEFINITION_TYPE = "definition_type"

  // training definition type
  val EXPERIMENT_TYPE = "experiment"
  val PIPELINE_TYPE = "pipeline"
  val MODEL_DEFINITION_TYPE = "model_definition"
  val FEDERATED_LEARNING_TYPE = "federated_learning"

  // api
  val API_PREFIX = "/ml/v4"

  // asset search query
  val TAGS_QUERY = "asset.tags"
  val SEARCH_ALL = "*:*"
}
