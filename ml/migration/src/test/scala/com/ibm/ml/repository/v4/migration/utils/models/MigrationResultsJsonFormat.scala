/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils.models

import com.ibm.analytics.wml.api.v4.common.CommonJsonFormat._
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

object MigrationResultsJsonFormat extends DefaultJsonProtocol {

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

  implicit val migrationResultsFormat: RootJsonFormat[MigrationResults] = jsonFormat3(MigrationResults.apply)

  implicit val migrationDocFormat: RootJsonFormat[MigrationOutcome] = jsonFormat(MigrationOutcome.apply,
    "status",
    "results"
  )

}
