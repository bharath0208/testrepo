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

trait PipelineMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen {

  "Testing pipeline migration" when {
    "PIPELINE_MIG_1: v4beta pipeline with library should be migrated to v4ga repository" in {

      Given("A v4Beta pipeline")
      val (newSpaceId, newProjectId) = createNewContainer()
      val libraryId = createV4BetaLibrary(spaceId = spaceId, projectId = projectId)
      val pipeline = createV4BetaPipeline(libraryId = Some(libraryId), spaceId = spaceId, projectId = projectId)

      When("Migration service is invoked for the v4Beta pipeline")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(pipelineIds = Some(Seq(pipeline.metadata.id)), newSpaceId = newSpaceId, newProjectId = newProjectId)
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

      And("v4Beta pipeline is migrated to the new repository")
      val maybePipeline = migrationOutcome.results.successful.find(_.newAssetType.equals("pipelines"))
      assert(maybePipeline.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaPipeline = client.pipelines.getById(id = maybePipeline.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("New pipeline contains a list of runtime definitions")
      assert(v4gaPipeline._1.entity.document.isDefined)
      val runtimes = v4gaPipeline._1.entity.document.get.runtimes
      assert(runtimes.isDefined)

      And("New pipeline contains a runtime with name 'DL' which has a software_spec for tensorflow_1.15-py3.6")
      val dlRuntime = runtimes.get.find(_.id == "DL").getOrElse(fail("runtime with id 'DL' expected but not found"))
      val tensorflowSwSpecId = softwareSpecifications.resources.find(_.metadata.name.getOrElse("") == "tensorflow_1.15-py3.6").getOrElse("software_spec list from environment API does not contain 'tensorflow_1.15-py3.6'")
      val dlRuntimeWmlData = dlRuntime
        .appData
        .getOrElse(fail("runtime with name 'DL' does not contain ApData element"))
        .wmlData
      dlRuntimeWmlData
        .softwareSpec
        .getOrElse(fail("runtime with name 'DL' does not contain software_spec"))
        .id.equals(tensorflowSwSpecId)

      And("New pipeline contains a runtime with id 'DL' which has a hardware_spec for 'k80'")
      val k80HwSpecId = hardwareSpecifications.resources.find(_.metadata.name.getOrElse("") == "K80").getOrElse("hardware_spec list from environment API does not contain 'K80'")
      dlRuntimeWmlData
        .hardwareSpec
        .getOrElse(fail("runtime with name 'DL' does not contain hardware_spec"))
        .id.equals(k80HwSpecId)

      And("New pipeline contains a runtime with id 'DL2' which has a software_spec for tensorflow_1.15-py3.6 - migrated from V3 runtime spec in the old pipeline")
      val dl2Runtime = runtimes.get.find(_.id == "DL2").getOrElse(fail("runtime with id 'DL2' expected but not found"))
      val dl2RuntimeWmlData = dl2Runtime
        .appData
        .getOrElse(fail("runtime with name 'DL2' does not contain ApData element"))
        .wmlData
      dl2RuntimeWmlData
        .softwareSpec
        .getOrElse(fail("runtime with name 'DL2' does not contain software_spec"))
        .id.equals(tensorflowSwSpecId)

      And("New pipeline contains a runtime with id 'automl' which has a hardware_spec of compute size 'M' - migrated from V4 runtime spec in the old pipeline")
      val automlRuntime = runtimes.get.find(_.id == "automl").getOrElse(fail("runtime with id 'DL' expected but not found"))
      val mHwSpecId = hardwareSpecifications.resources.find(_.metadata.name.getOrElse("") == "M").getOrElse("hardware_spec list from environment API does not contain 'M'")
      val automlRuntimeWmlData = automlRuntime
        .appData
        .getOrElse(fail("runtime with name 'automl' does not contain ApData element"))
        .wmlData
      automlRuntimeWmlData
        .hardwareSpec
        .getOrElse(fail("runtime with name 'automl' does not contain hardware_spec"))
        .id.equals(mHwSpecId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaPipeline._1.entity.custom)._1 == pipeline.metadata.id)
    }
  }

  "Testing pipeline migration when training_lib_href contains model_definition" when {
    "PIPELINE_MIG_2: v4beta pipeline with library should be migrated to v4ga repository" in {

      Given("A v4Beta pipeline")
      val (newSpaceId, newProjectId) = createNewContainer()
      val mDef = createV4BetaModelDefinition(spaceId = spaceId, projectId = projectId)
      val pipeline = createV4BetaPipeline(fileName = "upgrade.v4betapayloads/pipelines/create_pipeline_2.json", modelDefinitionId = mDef.metadata.assetId, spaceId = spaceId, projectId = projectId)

      When("Migration service is invoked for the v4Beta pipeline")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(pipelineIds = Some(Seq(pipeline.metadata.id)), newSpaceId = newSpaceId, newProjectId = newProjectId)
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

      And("v4Beta pipeline is migrated to the new repository")
      val maybePipeline = migrationOutcome.results.successful.find(_.newAssetType.equals("pipelines"))
      assert(maybePipeline.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaPipeline = client.pipelines.getById(id = maybePipeline.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("New pipeline contains a list of runtime definitions")
      assert(v4gaPipeline._1.entity.document.isDefined)
      val runtimes = v4gaPipeline._1.entity.document.get.runtimes
      assert(runtimes.isDefined)

      And("New pipeline contains a runtime with name 'DL' which has a software_spec for tensorflow_1.15-py3.6")
      val dlRuntime = runtimes.get.find(_.id == "DL").getOrElse(fail("runtime with id 'DL' expected but not found"))
      val tensorflowSwSpecId = softwareSpecifications.resources.find(_.metadata.name.getOrElse("") == "tensorflow_1.15-py3.6").getOrElse("software_spec list from environment API does not contain 'tensorflow_1.15-py3.6'")
      val dlRuntimeWmlData = dlRuntime
        .appData
        .getOrElse(fail("runtime with name 'DL' does not contain ApData element"))
        .wmlData
      dlRuntimeWmlData
        .softwareSpec
        .getOrElse(fail("runtime with name 'DL' does not contain software_spec"))
        .id.equals(tensorflowSwSpecId)

      And("New pipeline contains a runtime with id 'DL' which has a hardware_spec for 'k80'")
      val k80HwSpecId = hardwareSpecifications.resources.find(_.metadata.name.getOrElse("") == "K80").getOrElse("hardware_spec list from environment API does not contain 'K80'")
      dlRuntimeWmlData
        .hardwareSpec
        .getOrElse(fail("runtime with name 'DL' does not contain hardware_spec"))
        .id.equals(k80HwSpecId)

      And("New pipeline contains a runtime with id 'DL2' which has a software_spec for tensorflow_1.15-py3.6 - migrated from V3 runtime spec in the old pipeline")
      val dl2Runtime = runtimes.get.find(_.id == "DL2").getOrElse(fail("runtime with id 'DL2' expected but not found"))
      val dl2RuntimeWmlData = dl2Runtime
        .appData
        .getOrElse(fail("runtime with name 'DL2' does not contain ApData element"))
        .wmlData
      dl2RuntimeWmlData
        .softwareSpec
        .getOrElse(fail("runtime with name 'DL2' does not contain software_spec"))
        .id.equals(tensorflowSwSpecId)

      And("New pipeline contains a runtime with id 'automl' which has a hardware_spec of compute size 'M' - migrated from V4 runtime spec in the old pipeline")
      val automlRuntime = runtimes.get.find(_.id == "automl").getOrElse(fail("runtime with id 'DL' expected but not found"))
      val mHwSpecId = hardwareSpecifications.resources.find(_.metadata.name.getOrElse("") == "M").getOrElse("hardware_spec list from environment API does not contain 'M'")
      val automlRuntimeWmlData = automlRuntime
        .appData
        .getOrElse(fail("runtime with name 'automl' does not contain ApData element"))
        .wmlData
      automlRuntimeWmlData
        .hardwareSpec
        .getOrElse(fail("runtime with name 'automl' does not contain hardware_spec"))
        .id.equals(mHwSpecId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaPipeline._1.entity.custom)._1 == pipeline.metadata.id)
    }
  }
}
