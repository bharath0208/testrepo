/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.route

import akka.actor.ActorSystem
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob

case class MigrationRouteRegistry(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity) {
  val experiment: ExperimentMigrationRoute = ExperimentMigrationRoute(mj)
  val function: FunctionMigrationRoute = FunctionMigrationRoute(mj)
  val library: LibraryMigrationRoute = LibraryMigrationRoute(mj)
  val model: ModelMigrationRoute = ModelMigrationRoute(mj)
  val pipeline: PipelineMigrationRoute = PipelineMigrationRoute(mj)
  val runtime: RuntimeMigrationRoute = RuntimeMigrationRoute(mj)
  val trainingLib: TrainingLibMigrationRoute = TrainingLibMigrationRoute(mj)
  val modelDefinition: ModelDefinitionMigrationRoute = ModelDefinitionMigrationRoute(mj)
  val deploymentJobDefinition: DeploymentJobDefinitionMigrationRoute = DeploymentJobDefinitionMigrationRoute(mj)
}
