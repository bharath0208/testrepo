/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.api.v4.driver

import java.nio.charset.StandardCharsets
import java.util.{Base64, Date}

import com.ibm.ml.repository.v4.migration.cloudant.SimpleCloudantClient
import com.ibm.ml.repository.v4.migration.job.MLRepositoryMigrationApp
import com.ibm.ml.repository.v4.migration.models.{Canceled, Completed, Failed, MigrationDoc, OldInstance, Running}
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import org.apache.logging.log4j.{LogManager, Logger}
import scala.concurrent.{ExecutionContext, Future}
import spray.json._

case class DBTestDriver(cloudantClient: SimpleCloudantClient, migrationUserId: String, oldInstanceId: String) extends MigrationTestDriver {

  private val logger: Logger = LogManager.getLogger(classOf[DBTestDriver])

  override def startMigration(experimentIds: Option[Seq[String]] = None,
                               functionIds: Option[Seq[String]] = None,
                               pipelineIds: Option[Seq[String]] = None,
                               modelIds: Option[Seq[String]] = None,
                               spaceId: Option[String],
                               projectId: Option[String])(implicit ec: ExecutionContext): Future[MigrationDoc] = {
    val migrationDoc = MigrationDoc(
      userId = migrationUserId,
      oldInstance = OldInstance(oldInstanceId, None),
      status = Running,
      createdAt = new Date().getTime,
      modifiedAt = new Date().getTime,
      spaceId = spaceId,
      projectId = projectId,
      experimentIds = experimentIds,
      functionIds = functionIds,
      pipelineIds = pipelineIds,
      modelIds = modelIds
    )

    for {
      mDocJson <- cloudantClient.save(migrationDoc.toJson)
    } yield {
      val savedDoc = mDocJson.convertTo[MigrationDoc]
      logger.info(s"Created migrationJob with id: ${savedDoc.id}")
      startMigrationApp(savedDoc)
      savedDoc
    }
  }

  private def startMigrationApp(migrationDoc: MigrationDoc) = {
    val json = migrationDoc.toJson.toString()
    val jobDetails = new String(Base64.getEncoder.encode(json.getBytes(StandardCharsets.UTF_8)))
    MLRepositoryMigrationApp(Array(s"JOB_DETAILS=$jobDetails")).start()
  }

  override def trackMigrationJob(migrationId: String,
                                 spaceId: Option[String],
                                 projectId: Option[String])(implicit ec: ExecutionContext): Future[MigrationDoc] = {
    cloudantClient.find(migrationId).flatMap(doc => {
      val mDoc = doc.convertTo[MigrationDoc]
      if (mDoc.status != Completed && mDoc.status != Failed && mDoc.status != Canceled) {
        sleep()
        trackMigrationJob(migrationId, spaceId, projectId)
      } else {
        Future.successful(mDoc)
      }
    })
  }

  override def cleanupMigrationJob(migrationResult: MigrationDoc,
                                   spaceId: Option[String],
                                   projectId: Option[String])(implicit ec: ExecutionContext) = {
    for {
      latestDoc <- cloudantClient.find(migrationResult.id.getOrElse("ERROR_MIG_ID_NOT_FOUND")) // should not happen, checking in an earlier step that id is not None
      _ <- cloudantClient.remove(latestDoc.toJson)
    } yield {}
  }
}
