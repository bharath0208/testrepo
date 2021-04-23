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

trait FunctionMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing function migration" when {
    "FUNCTION_MIG_1: v4beta function with runtime should be migrated to v4ga repository" in {
      Given("A v4Beta function")
      val function = createV4BetaFunction()

      When("Migration service is invoked for the v4Beta function")
      val migrationResult = Await.result(for{
        migrationDoc <- startMigration(functionIds = Some(Seq(function.metadata.id)))
        _ <- futureSleep(200)
        migrationDoc <- trackMigrationJob(migrationDoc.id.getOrElse(fail("could not create migration job")))
      } yield {
        migrationDoc
      }, Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed, s"Migration did not complete: ${logPrint(migrationResult.toJson)}")

      And("Migration result has 1 success record")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 1)

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
      val maybeSpecification = softwareSpecifications.resources.find(_.metadata.assetId == swSpecRef.get.newId)
      assert(maybeSpecification.isDefined)
      assert(maybeSpecification.get.metadata.name.getOrElse("").equals("tensorflow_1.15-py3.6"))

      And("v4Beta function is migrated to the new repository")
      val maybeFunction = migrationResult.results.get.successful.find(_.newAssetType.equals("functions"))
      assert(maybeFunction.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaFunction = client.functions.getById(id = maybeFunction.get.newId, expectedStatus = Seq(200), spaceId = spaceId, projectId = projectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaFunction._1.entity.custom) == (function.metadata.id, "functions", oldInstanceId))
    }
  }
}
