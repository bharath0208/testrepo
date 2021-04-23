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

trait FunctionMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing function migration" when {
    "FUNCTION_MIG_1: v4beta function with runtime should be migrated to v4ga repository" in {
      Given("A v4Beta function")
      val (newSpaceId, newProjectId) = createNewContainer()
      val function = createV4BetaFunction(spaceId = spaceId, projectId = projectId);

      When("Migration service is invoked for the v4Beta function")
      val migrationDoc = Await.result((for{
        migrationDoc <- startMigration(functionIds = Some(Seq(function.metadata.id)), newSpaceId = newSpaceId, newProjectId = newProjectId)
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
      assert(migrationOutcome.results.successful.size == 1)

      And("Migration result has 1 skipped record")
      assert(migrationOutcome.results.skipped.size == 0)

      And("Migration result has 0 failed records")
      assert(migrationOutcome.results.failed.size == 0)

      And("v4Beta function is migrated to the new repository")
      val maybeFunction = migrationOutcome.results.successful.find(_.newAssetType.equals("functions"))
      assert(maybeFunction.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaFunction = client.functions.getById(id = maybeFunction.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = projectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaFunction._1.entity.custom)._1 == function.metadata.id)
    }
  }

}
