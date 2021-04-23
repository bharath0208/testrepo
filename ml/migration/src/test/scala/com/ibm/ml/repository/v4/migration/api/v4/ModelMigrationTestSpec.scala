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

trait ModelMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing model migration" when {
    "MODEL_MIG_1: v4beta model with pipeline should be migrated to v4ga repository" in {
      Given("A v4Beta model")
      val library = createV4BetaLibrary()
      val pipeline = createV4BetaPipeline(libraryId = Some(library.metadata.id))
      val model = createV4BetaModel(pipelineHref = pipeline.metadata.href)

      When("Migration service is invoked for the v4Beta model")
      val migrationResult = Await.result(for{
        migrationDoc <- startMigration(modelIds = Some(Seq(model.metadata.id)))
        _ <- futureSleep(200)
        migrationDoc <- trackMigrationJob(migrationDoc.id.getOrElse(fail("could not create migration job")))
      } yield {
        migrationDoc
      }, Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed, s"Migration did not complete: ${logPrint(migrationResult.toJson)}")

      And("Migration result has 1 success record")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 3)

      And("Migration result has 1 skipped record")
      val skipped = migrationResult.results.getOrElse(fail("migration results not available")).skipped
      assert(skipped.isDefined)
      assert(skipped.size == 1)

      And("Migration result has 0 failed records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).failed.isEmpty)

      And("v4Beta runtime is mapped to appropriate software_specification")
      val swSpecRef = migrationResult.results
        .get.skipped.getOrElse(fail("result.skipped expected but not found"))
        .find(_.newAssetType.equals("software_specifications"))
      assert(swSpecRef.isDefined)
      val swSpec = softwareSpecifications.resources.find(_.metadata.assetId == swSpecRef.get.newId)
      assert(swSpec.isDefined)
      assert(swSpec.get.metadata.name.getOrElse("").equals("tensorflow_1.15-py3.6"))

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

      And("v4Beta model is migrated to the new repository")
      val maybeModel = migrationResult.results.get.successful.find(_.newAssetType.equals("models"))
      assert(maybeModel.isDefined)
      //GET on the V4GA asset should return 200
      val newModel = client.models.getById(id = maybeModel.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)
      assert(newModel._1.metadata.id.equals(maybeModel.get.newId))

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(newModel._1.entity.custom) == (model.metadata.id, "models", oldInstanceId))
    }

    "MODEL_MIG_2: v4beta model with training_lib should be migrated to v4ga repository" in {
      Given("A v4Beta model")
      val library = createV4BetaLibrary()
      val model = createV4BetaModel(fileName = "v4betapayloads/models/create_model_with_training_lib.json", trainingLibHref = library.metadata.href)

      When("Migration service is invoked for the v4Beta model")
      val migrationResult = Await.result(for{
        migrationDoc <- startMigration(modelIds = Some(Seq(model.metadata.id)))
        _ <- futureSleep(200)
        migrationDoc <- trackMigrationJob(migrationDoc.id.getOrElse(fail("could not create migration job")))
      } yield {
        migrationDoc
      }, Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed, s"Migration did not complete: ${logPrint(migrationResult.toJson)}")

      And("Migration result has 1 success record")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 2)

      And("Migration result has 1 skipped record")
      val skipped = migrationResult.results.getOrElse(fail("migration results not available")).skipped
      assert(skipped.isDefined)
      assert(skipped.size == 1)

      And("Migration result has 0 failed records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).failed.isEmpty)

      And("v4Beta runtime is mapped to appropriate software_specification")
      val swSpecRef = migrationResult.results
        .get.skipped.getOrElse(fail("result.skipped expected but not found"))
        .find(_.newAssetType.equals("software_specifications"))
      assert(swSpecRef.isDefined)
      val swSpec = softwareSpecifications.resources.find(_.metadata.assetId == swSpecRef.get.newId)
      assert(swSpec.isDefined)
      assert(swSpec.get.metadata.name.getOrElse("").equals("tensorflow_1.15-py3.6"))

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationResult.results.get.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4Beta model is migrated to the new repository")
      val maybeModel = migrationResult.results.get.successful.find(_.newAssetType.equals("models"))
      assert(maybeModel.isDefined)
      //GET on the V4GA asset should return 200
      val newModel = client.models.getById(id = maybeModel.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)
      assert(newModel._1.metadata.id.equals(maybeModel.get.newId))

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(newModel._1.entity.custom) == (model.metadata.id, "models", oldInstanceId))
    }

    "MODEL_MIG_3: v4beta model with software_spec should be migrated to v4ga repository" in {
      Given("A v4Beta model")
      val library = createV4BetaLibrary()
      val model = createV4BetaModel(
        fileName = "v4betapayloads/models/create_model_with_software_spec.json",
        trainingLibHref = library.metadata.href,
        swSpecId = Some(softwareSpecifications.resources.head.metadata.assetId))

      When("Migration service is invoked for the v4Beta model")
      val migrationResult = Await.result(for{
        migrationDoc <- startMigration(modelIds = Some(Seq(model.metadata.id)))
        _ <- futureSleep(200)
        migrationDoc <- trackMigrationJob(migrationDoc.id.getOrElse(fail("could not create migration job")))
      } yield {
        migrationDoc
      }, Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed, s"Migration did not complete: ${logPrint(migrationResult.toJson)}")

      And("Migration result has 1 success record")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 2)

      And("Migration result has 0 skipped records")
      val skipped = migrationResult.results.getOrElse(fail("migration results not available")).skipped
      assert(skipped.isEmpty)

      And("Migration result has 0 failed records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).failed.isEmpty)

      And("v4Beta library is migrated to the new repository as model_definition")
      val maybeModelDef = migrationResult.results.get.successful.find(_.newAssetType.equals("model_definitions"))
      assert(maybeModelDef.isDefined)
      //GET on the V4GA asset should return 200
      client.modelDefinitions.getById(id = maybeModelDef.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4Beta model is migrated to the new repository")
      val maybeModel = migrationResult.results.get.successful.find(_.newAssetType.equals("models"))
      assert(maybeModel.isDefined)
      //GET on the V4GA asset should return 200
      val newModel = client.models.getById(id = maybeModel.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)
      assert(newModel._1.metadata.id.equals(maybeModel.get.newId))

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(newModel._1.entity.custom) == (model.metadata.id, "models", oldInstanceId))
    }
  }
}
