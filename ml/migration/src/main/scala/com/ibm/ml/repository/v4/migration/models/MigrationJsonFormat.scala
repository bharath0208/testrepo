/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.models

import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
import spray.json._

object MigrationJsonFormat extends DefaultJsonProtocol {
  implicit val migrationStatusFormat: MigrationStatusFormat.type = MigrationStatusFormat

  implicit val oldInstanceFormat: RootJsonFormat[OldInstance] = jsonFormat(OldInstance.apply,
    "instance_id",
    "api_key"
  )

  implicit val migrationSuccessfulResultFormat: RootJsonFormat[MigrationSuccessfulResult] = jsonFormat(MigrationSuccessfulResult.apply,
    "old_asset_type",
    "new_asset_type",
    "old_id",
    "new_id",
    "asset_name",
    "old_modified_at"
  )

  implicit val migrationFailedResultFormat: RootJsonFormat[MigrationFailedResult] = jsonFormat(MigrationFailedResult.apply,
    "old_asset_type",
    "old_id",
    "asset_name",
    "reason",
    "old_modified_at"
  )

  implicit val migrationIdsFormat: RootJsonFormat[MigrationIds] = jsonFormat(MigrationIds.apply,
    "experiment_ids",
    "function_ids",
    "model_ids",
    "pipeline_ids",
    "training_library_ids",
    "runtime_ids",
    "library_ids"
  )

  implicit val migrationResultsFormat: RootJsonFormat[MigrationResults] = jsonFormat3(MigrationResults.apply)

  implicit val migrationRequestFormat: RootJsonFormat[MigrationRequest] = jsonFormat(MigrationRequest.apply,
    "old_instance",
    "space_id",
    "project_id",
    "experiment_ids",
    "function_ids",
    "model_ids",
    "pipeline_ids",
    "mapping",
    "skip_migrated_assets"
  )

  implicit val migrationResourceFormat: RootJsonFormat[MigrationResource] = jsonFormat(MigrationResource.apply,
    "migration_id",
    "status",
    "results",
    "console",
    "mapping",
    "old_instance",
    "space_id",
    "project_id",
    "created_at",
    "modified_at"
  )

  implicit val migrationResourcesFormat: RootJsonFormat[MigrationResources] = jsonFormat1(MigrationResources.apply)

  implicit val migrationDocFormat: RootJsonFormat[MigrationDoc] = jsonFormat(MigrationDoc.apply,
    "user_id",
    "status",
    "created_at",
    "modified_at",
    "old_instance",
    "_id",
    "_rev",
    "results",
    "console",
    "mapping",
    "space_id",
    "project_id",
    "experiment_ids",
    "function_ids",
    "model_ids",
    "pipeline_ids",
    "skip_migrated_assets"
  )

  implicit val nextJobIdFormat: RootJsonFormat[NextJobId] = jsonFormat(NextJobId.apply,
    "created_at",
    "id"
  )

  object MigrationStatusFormat extends RootJsonFormat[MigrationStatus] {

    override def read(json: JsValue): MigrationStatus = {
      json match {
        case JsString(v) => MigrationStatusFromStr(v)
        case v => deserializationError(s"Expected string but found $v")
      }
    }

    def MigrationStatusFromStr(name: String): MigrationStatus = {
      name.toLowerCase match {
        case Failed.name => Failed
        case Pending.name => Pending
        case Initializing.name => Initializing
        case Running.name => Running
        case Completed.name => Completed
        case Canceled.name => Canceled
        case _ => deserializationError(s"Unsupported migration status $name")
      }
    }

    override def write(obj: MigrationStatus): JsString = {
      JsString(obj.name)
    }
  }

  import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat.metaDataFormat

  implicit val v4BetaResourceFormat: RootJsonFormat[V4BetaResource] = jsonFormat1(V4BetaResource.apply)

  import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat.hyperReferenceFormat

  implicit val v4BetaResourcesFormat: RootJsonFormat[V4BetaResources] = jsonFormat4(V4BetaResources.apply)

  implicit val migratedFromFormat: RootJsonFormat[MigratedFrom] = jsonFormat(MigratedFrom.apply,
    "instance_id",
    "asset_type",
    "asset_id"
  )
}
