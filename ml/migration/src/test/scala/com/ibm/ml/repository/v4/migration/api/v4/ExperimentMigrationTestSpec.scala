/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.api.v4

import com.ibm.analytics.wml.repository.ml.migration.api.v4.MLRepoMigrationTestBase
import com.ibm.ml.repository.v4.migration.models.Completed
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat.migrationDocFormat
import org.scalatest.GivenWhenThen
import spray.json._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait ExperimentMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing experiment migration" when {
    "EXP_MIG_1: v4beta experiment with pipeline containing library under training_references should be migrated to v4ga repository" in {
      Given("A v4Beta experiment")
      val library = createV4BetaLibrary()
      val pipeline = createV4BetaPipeline(libraryId = Some(library.metadata.id))
      val experimentId = createV4BetaExperiment(pipelineHref = pipeline.metadata.href)

      When("Migration service is invoked for the v4Beta experiment")
      val migrationResult = Await.result(for{
        migrationDoc <- startMigration(experimentIds = Some(Seq(experimentId)))
        _ <- futureSleep(200)
        migrationDoc <- trackMigrationJob(migrationDoc.id.getOrElse(fail("could not create migration job")))
      } yield {
        migrationDoc
      }, Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed, s"Migration did not complete: ${logPrint(migrationResult.toJson)}")

      And("Migration result has 2 success records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 3)

      And("Migration result has 0 failed records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).failed.isEmpty)

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationResult.results.get.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4Beta pipeline is migrated to the new repository")
      val maybePipeline = migrationResult.results.get.successful.find(_.newAssetType.equals("pipelines"))
      assert(maybePipeline.isDefined)
      //GET on the V4GA asset should return 200
      client.pipelines.getById(id = maybePipeline.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4Beta experiment is migrated to the new repository")
      val maybeExperiment = migrationResult.results.get.successful.find(_.newAssetType.equals("experiments"))
      assert(maybeExperiment.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaExperiment = client.experiments.getById(id = maybeExperiment.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaExperiment._1.entity.custom) == (experimentId, "experiments", oldInstanceId))
    }

    "EXP_MIG_2: v4beta experiment with library under training_references should be migrated to v4ga repository" in {
      Given("A v4Beta experiment")
      val library = createV4BetaLibrary()
      val experimentId = createV4BetaExperiment(
        fileName = "v4betapayloads/experiments/create_experiment_2.json",
        libraryHref = library.metadata.href,
        libraryId = Some(library.metadata.id),
        spaceId = spaceId,
        projectId = projectId)

      When("Migration service is invoked for the v4Beta experiment")
      val migrationResult = Await.result(for{
        migrationDoc <- startMigration(experimentIds = Some(Seq(experimentId)))
        _ <- futureSleep(200)
        migrationDoc <- trackMigrationJob(migrationDoc.id.getOrElse(fail("could not create migration job")))
      } yield {
        migrationDoc
      }, Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed, s"Migration did not complete: ${logPrint(migrationResult.toJson)}")

      And("Migration result has 2 success records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 2)

      And("Migration result has 0 failed records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).failed.isEmpty)

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationResult.results.get.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4Beta experiment is migrated to the new repository")
      val maybeExperiment = migrationResult.results.get.successful.find(_.newAssetType.equals("experiments"))
      assert(maybeExperiment.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaExperiment = client.experiments.getById(id = maybeExperiment.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaExperiment._1.entity.custom) == (experimentId, "experiments", oldInstanceId))
    }
  }
}
