/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.api.v4

import com.ibm.ml.repository.v4.migration.models.Completed
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.utils.models.{MigrationOutcome, MigrationResults}
import org.scalatest.GivenWhenThen

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait ExperimentMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing experiment migration" when {
    s"$containerType|$authTokenType EXP_MIG_1: v4beta experiment with pipeline containing library under training_references should be migrated to v4ga repository" in {
      Given("A v4Beta experiment")
      val (newSpaceId, newProjectId) = createNewContainer()
      val libraryId = createV4BetaLibrary(spaceId = spaceId, projectId = projectId)
      val pipeline = createV4BetaPipeline(libraryId = Some(libraryId), spaceId = spaceId, projectId = projectId)
      val experimentId = createV4BetaExperiment(pipelineHref = pipeline.metadata.href, spaceId = spaceId, projectId = projectId)

      When("Migration service is invoked for the v4Beta experiment")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(experimentIds = Some(Seq(experimentId)), newSpaceId = newSpaceId, newProjectId = newProjectId)
        _ <- futureSleep(200)
        latestMigrationDoc <- trackMigrationJob(migrationDoc.asJsObject.fields("_id").convertTo[String])
      } yield {
        latestMigrationDoc
      }), Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      val status = migrationDoc.asJsObject.fields("status").convertTo[String]
      assert(status == Completed.name)
      import com.ibm.ml.repository.v4.migration.utils.models.MigrationResultsJsonFormat._
      val migrationResults = migrationDoc.asJsObject.fields("results").convertTo[MigrationResults]
      val migrationOutcome = MigrationOutcome(status, migrationResults)

      And("Migration result has 2 success records")
      assert(migrationOutcome.results.successful.size == 3)

      And("Migration result has 0 failed records")
      assert(migrationOutcome.results.failed.size == 0)

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationOutcome.results.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4Beta pipeline is migrated to the new repository")
      val maybePipeline = migrationOutcome.results.successful.find(_.newAssetType.equals("pipelines"))
      assert(maybePipeline.isDefined)
      //GET on the V4GA asset should return 200
      client.pipelines.getById(id = maybePipeline.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4Beta experiment is migrated to the new repository")
      val maybeExperiment = migrationOutcome.results.successful.find(_.newAssetType.equals("experiments"))
      assert(maybeExperiment.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaExperiment = client.experiments.getById(id = maybeExperiment.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaExperiment._1.entity.custom)._1 == experimentId)
    }

    s"$containerType|$authTokenType EXP_MIG_2: v4beta experiment with library under training_references should be migrated to v4ga repository" in {
      Given("A v4Beta experiment")
      val (newSpaceId, newProjectId) = createNewContainer()
      val libraryId = createV4BetaLibrary(spaceId = spaceId, projectId = projectId)
      val experimentId = createV4BetaExperiment(
        fileName = "upgrade.v4betapayloads/experiments/create_experiment_2.json",
        libraryHref = Some(s"/v4/libraries/$libraryId"),
        libraryId = Some(libraryId),
        spaceId = spaceId,
        projectId = projectId)

      When("Migration service is invoked for the v4Beta experiment")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(experimentIds = Some(Seq(experimentId)), newSpaceId = newSpaceId, newProjectId = newProjectId)
        _ <- futureSleep(200)
        latestMigrationDoc <- trackMigrationJob(migrationDoc.asJsObject.fields("_id").convertTo[String])
      } yield {
        latestMigrationDoc
      }), Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      val status = migrationDoc.asJsObject.fields("status").convertTo[String]
      assert(status == Completed.name)
      import com.ibm.ml.repository.v4.migration.utils.models.MigrationResultsJsonFormat._
      val migrationResults = migrationDoc.asJsObject.fields("results").convertTo[MigrationResults]
      val migrationOutcome = MigrationOutcome(status, migrationResults)

      And("Migration result has 2 success records")
      assert(migrationOutcome.results.successful.size == 2)

      And("Migration result has 0 failed records")
      assert(migrationOutcome.results.failed.size == 0)

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationOutcome.results.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4Beta experiment is migrated to the new repository")
      val maybeExperiment = migrationOutcome.results.successful.find(_.newAssetType.equals("experiments"))
      assert(maybeExperiment.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaExperiment = client.experiments.getById(id = maybeExperiment.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaExperiment._1.entity.custom)._1 == experimentId)
    }
  }
}
