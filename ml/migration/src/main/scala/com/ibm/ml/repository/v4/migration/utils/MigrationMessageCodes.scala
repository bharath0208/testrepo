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

import com.ibm.ml.repository.v4.utils.MessageCode

/** Failed to create the service */
case class MLRepositoryMigrationServiceFailed() extends MessageCode {
  override val code = "ml_migration_service_failed"
  override val message = "Failed to create the V4 repository migration service"
}

/** Failed to create the App */
case class MLRepositoryMigrationAppFailed() extends MessageCode {
  override val code = "ml_migration_app_failed"
  override val message = "Failed to create the V4 repository migration app"
}

/** Failed to create the endpoints */
case class EndpointCreationFailed() extends MessageCode {
  override val code = "ml_migration_service_creation_failed"
  override val message = "Failed to create the V4 repository migration service"
}

case class MigrationJobRejected(message: String) extends MessageCode {
  override val code: String = "ml_migration_job_rejected"
}
