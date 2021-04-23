/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils

trait MigrationConstant {
  val CLOUDANT_DESIGN_DOC_PATH = "design_doc.json"
  val CLOUDANT_UPGRADE_DESIGN_DOC_PATH = "upgrade_design_doc.json"
  val CLOUDANT_DESIGN_DOC_NAME = "design_doc"
  val GET_NEXT_JOB_VIEW = "get-next-job"
  val GET_RUNNING_JOB_VIEW = "get-running-jobs"
  val GET_NON_TERMINATED_JOB_VIEW = "get-non-terminated-jobs"
  val GET_ALL_JOBS_VIEW = "get-all-jobs"
  val LIST_JOB_BY_USER_ID_VIEW = "list-job-by-userid"
  val LIST_JOB_BY_PROJECT_ID_VIEW = "list-job-by-project-id"
  val LIST_JOB_BY_SPACE_ID_VIEW = "list-job-by-space-id"
  val LIST_JOB_BY_STATUS_VIEW = "list-job-by-status"
  val MIGRATION_JOB_ID = "MIGRATION_JOB_ID"
  val OLD_INSTANCE_ID = "OLD_INSTANCE_ID"
  val OLD_INSTANCE_API_KEY = "OLD_INSTANCE_API_KEY"
  val JOB_DETAILS = "JOB_DETAILS"

  val ML_REPO_VERSION = "2020-08-01"
  val ASSET_ID_ALL = "all"

  val ARTIFACT_KB = "kb"
  val ARTIFACT_AUTOML = "autoai"
  val ARTIFACT_OBM = "obm"
  val ARTIFACT_PIPELINE_MODEL = "pipeline_model"
  val MODEL_TYPE_HYBRID = "wml-hybrid"
  val CONTENT_TYPE_NATIVE = "native"
  val CONTENT_TYPE_PIPELINE_NODE = "pipeline-node"
  val CONTENT_TYPE_CORE_ML = "coreML"

  val FIELD_IMPORT = "import"
  val FIELD_CONTENT_STATUS = "content_status"
  val FIELD_CONTENT_FORMAT = "content_formats"
  val FIELD_ID = "id"
  val FIELD_SPACE = "space"
  val FIELD_PROJECT = "project"
  val FIELD_SPACE_ID = "space_id"
  val FIELD_PROJECT_ID = "project_id"
  val FIELD_PIPELINE = "pipeline"
  val FIELD_PIPELINES = "pipelines"
  val FIELD_MODEL_DEFINITION = "model_definition"
  val FIELD_SOFTWARE_SPECIFICATION = "software_specification"
  val FIELD_HREF = "href"
  val FIELD_HARDWARE_SPECIFICATION = "hardware_specification"
  val FIELD_TRAINING_REFERENCES = "training_references"
  val FIELD_WML_DATA = "wml_data"
  val FIELD_APP_DATA = "app_data"
  val FIELD_SCHEMAS = "schemas"
  val FIELD_NAME = "name"
  val FIELD_TAGS = "tags"
  val FIELD_REV = "rev"
  val FIELD_COMPUTE = "compute"
  val FIELD_RUNTIME = "runtime"
  val FIELD_RUNTIMES = "runtimes"
  val FIELD_SOFTWARE_SPEC = "software_spec"
  val FIELD_HARDWARE_SPEC = "hardware_spec"
  val FIELD_HYBRID_PIPELINE_HARDWARE_SPECS = "hybrid_pipeline_hardware_specs"
  val FIELD_TRAINING_LIB = "training_lib"
  val FIELD_TRAINING_LIB_HREF = "training_lib_href"
  val FIELD_MODEL_DEF_URL = "model_definition_url"
  val FIELD_DOCUMENT = "document"
  val FIELD_TRAINING_DATA_REFERENCES = "training_data_references"
  val FIELD_TYPE = "type"
  val FIELD_SCORING = "scoring"

  val AUTO_AI_KB = "auto_ai.kb"
  val AUTO_AI_OBM = "auto_ai.obm"


  val DEFAULT_FUNCTION_SOFTWARE_SPEC = "ai-function_0.1-py3.6"
  val DEFAULT_MODEL_SOFTWARE_SPEC = "default_py3.7"

  val AUTO_AI_KB_SW_SPEC_NAME = "autoai-kb_3.1-py3.7"
  val AUTO_AI_OBM_SW_SPEC_NAME = "autoai-obm_2.0"

  val EXPERIMENTS_TYPE = "experiments"
  val RUNTIMES_TYPE = "runtimes"
  val TRAINING_LIBS_TYPE = "training_libs"
  val MODEL_DEFINITIONS_TYPE = "model_definitions"
  val PIPELINES_TYPE = "pipelines"
  val FUNCTIONS_TYPE = "functions"
  val LIBRARIES_TYPE = "libraries"
  val PACKAGE_EXTENSIONS_TYPE = "package_extensions"
  val MODELS_TYPE = "models"
  val SOFTWARE_SPECIFICATIONS_TYPE = "software_specifications"
  val DEPLOYMENT_JOB_DEFINITIONS_TYPE = "deployment_job_definitions"

  val WML_EXPERIMENT = "wml_experiment"
  val WML_PIPELINE = "wml_pipeline"
  val WML_FUNCTION = "wml_function"
  val WML_MODEL = "wml_model"
  val WML_MODEL_DEFINITION = "wml_model_definition"
  val WML_DEPLOYMENT_JOB_DEFINITION = "wml_deployment_job_definition"


  val SOFTWARE_SPECIFICATIONS_BASE_URI = "/v2/software_specifications"
  val PACKAGE_EXTENSION_BASE_URI = "/v2/package_extensions"
  val SOFTWARE_SPECIFICATION_TYPE_BASE = "base"
  val OP_DL_TRAIN = "dl_train"
  val V3_COMPUTE_CONFIG_NAME = "compute_configuration_name"
  val V3_COMPUTE_CONFIG_NODES = "compute_configuration_nodes"

  val RUNTIME_SOFTWARE_SPEC_MAPPING_PATH = "service.ml-repository.migration.runtimes"
  val COMPUTE_HARDWARE_SPEC_MAPPING_PATH = "service.ml-repository.migration.compute"
  val SOFTWARE_SPEC_UPGRADE_PUBLIC_MAPPING_PATH = "service.ml-repository.migration.software-specs.public"
  val SOFTWARE_SPEC_UPGRADE_COMMON_MAPPING_PATH = "service.ml-repository.migration.software-specs.common"
  val SOFTWARE_SPEC_UPGRADE_PRIVATE_MAPPING_PATH = "service.ml-repository.migration.software-specs.private"
  val MODEL_TYPE_UPGRADE_PUBLIC_MAPPING_PATH = "service.ml-repository.migration.model-types.public"
  val MODEL_TYPE_UPGRADE_COMMON_MAPPING_PATH = "service.ml-repository.migration.model-types.common"
  val MODEL_TYPE_UPGRADE_PRIVATE_MAPPING_PATH = "service.ml-repository.migration.model-types.private"

  val DEFAULT_PURGE_ON_DELETE = true

  val MAX_MIGRATE_VERSION = "4.0.0"

}
