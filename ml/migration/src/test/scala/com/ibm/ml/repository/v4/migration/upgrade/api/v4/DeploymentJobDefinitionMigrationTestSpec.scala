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

import com.ibm.ml.repository.v4.migration.upgrade.models.{Completed, MigrationDoc}
import org.scalatest.GivenWhenThen
import spray.json.DefaultJsonProtocol._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait DeploymentJobDefinitionMigrationTestSpec extends MLRepoMigrationTestBase with GivenWhenThen{

  "Testing deploymentJobDefinition migration" when {
    "DJD_MIG_1: v4beta deploymentJobDefinition should be migrated to v4ga repository" in {
      Given("A v4Beta deploymentJobDefinition")
      val deploymentJobDefinitionId = createV4BetaDeploymentJobDefinition(spaceId = spaceId, projectId = projectId).metadata.assetId
      val (newSpaceId, newProjectId) = createNewContainer()

      When("Migration service is invoked for the v4Beta deploymentJobDefinition")
      val migrationResult = Await.result((for{
        migrationDoc <- startMigration(deploymentJobDefinitionIds = Some(Seq(deploymentJobDefinitionId.get)), newSpaceId = newSpaceId, newProjectId = newProjectId)
        _ <- futureSleep(200)
        latestMigrationDoc <- trackMigrationJob(migrationDoc.asJsObject.fields("_id").convertTo[String])
      } yield {
        import com.ibm.ml.repository.v4.migration.upgrade.models.MigrationJsonFormat._
        latestMigrationDoc.convertTo[MigrationDoc]
      }), Duration.create(MAX_ALLOWED_MIGRATION_DURATION))

      Then("Migration status is Completed")
      assert(migrationResult.status == Completed)

      And("Migration result has 1 success records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).successful.size == 1)

      And("Migration result has 0 failed records")
      assert(migrationResult.results.getOrElse(fail("migration results not available")).failed.size == 0)

      And("v4Beta deploymentJobDefinition is migrated to the new repository")
      val maybeDeploymentJobDef = migrationResult.results.get.successful.find(_.newAssetType.equals("deployment_job_definitions"))
      assert(maybeDeploymentJobDef.isDefined)
      //GET on the V4GA asset should return 200
      val v4gaDeploymentJobDef = client.deploymentJobDefinitions.getById(id = maybeDeploymentJobDef.get.newId, expectedStatus = Seq(200), spaceId = newSpaceId, projectId = newProjectId)

      And("v4ga asset contains 'migrated_from' custom field with fields 'asset_id', 'asset_type', 'instance_id'")
      assert(getMigratedFrom(v4gaDeploymentJobDef._1.entity.custom)._1 == deploymentJobDefinitionId.get)
    }
  }

}
