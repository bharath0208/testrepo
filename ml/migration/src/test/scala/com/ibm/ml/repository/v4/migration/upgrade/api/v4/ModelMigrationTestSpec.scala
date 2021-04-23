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

trait ModelMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing model migration" when {
    "MODEL_MIG_1: v4beta model with pipeline should be migrated to v4ga repository" in {
      Given("A v4Beta model")
      val (newSpaceId, newProjectId) = createNewContainer()
      val libraryId = createV4BetaLibrary(spaceId = spaceId, projectId = projectId)
      val pipeline = createV4BetaPipeline(libraryId = Some(libraryId), spaceId = spaceId, projectId = projectId)
      val model = createV4BetaModel(pipelineHref = pipeline.metadata.href, spaceId = spaceId, projectId = projectId)

      When("Migration service is invoked for the v4Beta model")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(modelIds = Some(Seq(model.metadata.id)), newSpaceId = newSpaceId, newProjectId = newProjectId)
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

      And("Migration result has 1 success record")
      assert(migrationOutcome.results.successful.size == 3)

      And("Migration result has 1 skipped record")
      val skipped = migrationOutcome.results.skipped
      assert(skipped.size == 1)

      And("Migration result has 0 failed records")
      assert(migrationOutcome.results.failed.size == 0)

      And("v4Beta runtime is mapped to appropriate software_specification")
      val swSpecRef = migrationOutcome.results
        .skipped.find(_.newAssetType.equals("software_specifications"))
      assert(swSpecRef.isDefined)
      val swSpec = softwareSpecifications.resources.find(_.metadata.assetId == swSpecRef.get.newId)
      assert(swSpec.isDefined)
      assert(swSpec.get.metadata.name.getOrElse("").equals("tensorflow_1.15-py3.6"))

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

      And("v4Beta model is migrated to the new repository")
      val maybeModel = migrationOutcome.results.successful.find(_.newAssetType.equals("models"))
      assert(maybeModel.isDefined)
      //GET on the V4GA asset should return 200
      val newModel = client.models.getById(id = maybeModel.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)
      assert(newModel._1.metadata.id.equals(maybeModel.get.newId))

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(newModel._1.entity.custom)._1 == model.metadata.id)
    }

    "MODEL_MIG_2: v4beta model with training_lib should be migrated to v4ga repository" in {
      Given("A v4Beta model")
      val (newSpaceId, newProjectId) = createNewContainer()
      val libraryId = createV4BetaLibrary(spaceId = spaceId, projectId = projectId)
      val model = createV4BetaModel(
        fileName = "upgrade.v4betapayloads/models/create_model_with_training_lib.json",
        trainingLibHref = Some(s"/v4/libraries/$libraryId"),
        spaceId = spaceId,
        projectId = projectId)

      When("Migration service is invoked for the v4Beta model")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(modelIds = Some(Seq(model.metadata.id)), newSpaceId = newSpaceId, newProjectId = newProjectId)
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

      And("Migration result has 1 success record")
      assert(migrationOutcome.results.successful.size == 2)

      And("Migration result has 1 skipped record")
      val skipped = migrationOutcome.results.skipped
      assert(skipped.size == 1)

      And("Migration result has 0 failed records")
      assert(migrationOutcome.results.failed.size == 0)

      And("v4Beta runtime is mapped to appropriate software_specification")
      val swSpecRef = migrationOutcome.results.skipped.find(_.newAssetType.equals("software_specifications"))
      assert(swSpecRef.isDefined)
      val swSpec = softwareSpecifications.resources.find(_.metadata.assetId == swSpecRef.get.newId)
      assert(swSpec.isDefined)
      assert(swSpec.get.metadata.name.getOrElse("").equals("tensorflow_1.15-py3.6"))

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationOutcome.results.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4Beta model is migrated to the new repository")
      val maybeModel = migrationOutcome.results.successful.find(_.newAssetType.equals("models"))
      assert(maybeModel.isDefined)
      //GET on the V4GA asset should return 200
      val newModel = client.models.getById(id = maybeModel.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)
      assert(newModel._1.metadata.id.equals(maybeModel.get.newId))

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(newModel._1.entity.custom)._1 == model.metadata.id)
    }

    "MODEL_MIG_3: v4beta model with software_spec should be migrated to v4ga repository" in {
      Given("A v4Beta model")
      val (newSpaceId, newProjectId) = createNewContainer()
      val libraryId = createV4BetaLibrary(spaceId = spaceId, projectId = projectId)
      val model = createV4BetaModel(
        fileName = "upgrade.v4betapayloads/models/create_model_with_software_spec.json",
        trainingLibHref = Some(s"/v4/libraries/$libraryId"),
        swSpecId = Some(softwareSpecifications.resources.head.metadata.assetId),
        spaceId = spaceId,
        projectId = projectId)

      When("Migration service is invoked for the v4Beta model")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(modelIds = Some(Seq(model.metadata.id)), newSpaceId = newSpaceId, newProjectId = newProjectId)
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

      And("Migration result has 1 success record")
      assert(migrationOutcome.results.successful.size == 2)

      And("Migration result has 0 skipped records")
      val skipped = migrationOutcome.results.skipped
      assert(skipped.isEmpty)

      And("Migration result has 0 failed records")
      assert(migrationOutcome.results.failed.size == 0)

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationOutcome.results.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4Beta model is migrated to the new repository")
      val maybeModel = migrationOutcome.results.successful.find(_.newAssetType.equals("models"))
      assert(maybeModel.isDefined)
      //GET on the V4GA asset should return 200
      val newModel = client.models.getById(id = maybeModel.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)
      assert(newModel._1.metadata.id.equals(maybeModel.get.newId))

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(newModel._1.entity.custom)._1 == model.metadata.id)
    }
  }
}
