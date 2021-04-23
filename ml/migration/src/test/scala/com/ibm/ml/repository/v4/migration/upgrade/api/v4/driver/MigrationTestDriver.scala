/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.api.v4.driver

import com.ibm.ml.repository.v4.migration.models.MigrationDoc
import com.ibm.ml.repository.v4.migration.upgrade
import spray.json.JsValue

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

trait MigrationTestDriver {

  def startMigration(experimentIds: Option[Seq[String]] = None,
                     functionIds: Option[Seq[String]] = None,
                     pipelineIds: Option[Seq[String]] = None,
                     modelIds: Option[Seq[String]] = None,
                     spaceId: Option[String],
                     projectId: Option[String])(implicit ec: ExecutionContext): Future[MigrationDoc]

  def trackMigrationJob(migrationId: String,
                        spaceId: Option[String],
                        projectId: Option[String])(implicit ec: ExecutionContext): Future[JsValue]

  def cleanupMigrationJob(id: String,
                          spaceId: Option[String],
                          projectId: Option[String])(implicit ec: ExecutionContext): Future[Unit]

  protected def sleep(msecs: Long = 100): Unit = {
    Try(Thread.sleep(msecs))
  }

  def startMigrationUpgrade(experimentIds: Option[Seq[String]] = None,
                            functionIds: Option[Seq[String]] = None,
                            pipelineIds: Option[Seq[String]] = None,
                            modelIds: Option[Seq[String]] = None,
                            modelDefinitionIds: Option[Seq[String]] = None,
                            deploymentJobDefinitionIds: Option[Seq[String]] = None,
                            oldSpaceId: Option[String],
                            oldProjectId: Option[String],
                            newSpaceId: Option[String],
                            newProjectId: Option[String])(implicit ec: ExecutionContext): Future[upgrade.models.MigrationDoc]
}
