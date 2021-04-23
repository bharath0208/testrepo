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

import java.nio.charset.StandardCharsets
import java.util.{Base64, Date}

import akka.actor.ActorSystem
import com.ibm.analytics.wml.service.utils.security.BasicCredentials
import com.ibm.ml.repository.v4.migration.cloudant.SimpleCloudantClient
import com.ibm.ml.repository.v4.migration.job.MLRepositoryMigrationApp
import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
import com.ibm.ml.repository.v4.migration.models._
import com.ibm.ml.repository.v4.migration.upgrade
import com.ibm.ml.repository.v4.migration.utils.TestCloudantClient
import com.ibm.ml.repository.v4.utils.config
import org.apache.logging.log4j.{LogManager, Logger}
import spray.json._

import scala.concurrent.{ExecutionContext, Future}

object DBTestDriver {
  def apply(migrationUserId: String, oldInstanceId: String): DBTestDriver = {
    val (dbUrl, dbUsername, dbPassword, dbName) = SimpleCloudantClient.getDBConfig(config)

    val cloudantClient = TestCloudantClient(dbUrl, dbName, dbUsername, dbPassword, None, true)(ActorSystem())
    val upgradeCloudantClient = TestCloudantClient(dbUrl, dbName, dbUsername, dbPassword, None, true)(ActorSystem())
    DBTestDriver(migrationUserId, oldInstanceId, cloudantClient, upgradeCloudantClient)
  }
}

case class DBTestDriver(migrationUserId: String, oldInstanceId: String, cloudantClient: TestCloudantClient, upgradeCloudantClient: TestCloudantClient) extends MigrationTestDriver {

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
      val json = savedDoc.toJson.toString()
      val jobDetails = new String(Base64.getEncoder.encode(json.getBytes(StandardCharsets.UTF_8)))
      MLRepositoryMigrationApp(Array(s"JOB_DETAILS=$jobDetails")).start()
      savedDoc
    }
  }

  override def startMigrationUpgrade(experimentIds: Option[Seq[String]] = None,
                                     functionIds: Option[Seq[String]] = None,
                                     pipelineIds: Option[Seq[String]] = None,
                                     modelIds: Option[Seq[String]] = None,
                                     modelDefinitionIds: Option[Seq[String]] = None,
                                     deploymentJobDefinitionIds: Option[Seq[String]] = None,
                                     oldSpaceId: Option[String],
                                     oldProjectId: Option[String],
                                     newSpaceId: Option[String],
                                     newProjectId: Option[String])(implicit ec: ExecutionContext): Future[upgrade.models.MigrationDoc] = {
    val migrationDoc = upgrade.models.MigrationDoc(
      userId = Some(migrationUserId),
      status = upgrade.models.Running,
      createdAt = new Date().getTime,
      modifiedAt = new Date().getTime,
      oldSpaceId = oldSpaceId,
      oldProjectId = oldProjectId,
      newSpaceId = newSpaceId,
      newProjectId = newProjectId,
      experimentIds = experimentIds,
      functionIds = functionIds,
      pipelineIds = pipelineIds,
      modelIds = modelIds,
      modelDefinitionIds = modelDefinitionIds,
      deploymentJobDefinitionIds = deploymentJobDefinitionIds
    )

    import com.ibm.ml.repository.v4.migration.upgrade.models.MigrationJsonFormat._
    val docJson = migrationDoc.toJson
    for {
      mDocJson <- upgradeCloudantClient.save(docJson)
      identity <- {
        val basicCredentials = config.getString("service.platform.icp.token").trim
        val decoded = new String(Base64.getDecoder.decode(basicCredentials.getBytes("UTF-8"))).trim
        val up = decoded.split(":")
        BasicCredentials(up(0), up(1)).validate()
      }
    } yield {
      val savedDoc = mDocJson.convertTo[upgrade.models.MigrationDoc]
      logger.info(s"Created migrationJob with id: ${savedDoc.id}")
      val json = savedDoc.toJson.toString()
      val jobDetails = new String(Base64.getEncoder.encode(json.getBytes(StandardCharsets.UTF_8)))

      upgrade.job.MLRepositoryMigrationApp(Array(s"JOB_DETAILS=$jobDetails")).startJob(identity, savedDoc)
      savedDoc
    }
  }

  override def trackMigrationJob(migrationId: String,
                                 spaceId: Option[String],
                                 projectId: Option[String])(implicit ec: ExecutionContext): Future[JsValue] = {
    cloudantClient.find(migrationId).flatMap(doc => {
      import DefaultJsonProtocol._
      val status = doc.asJsObject.fields("status").convertTo[String]
      //val mDoc = doc.convertTo[MigrationDoc]
      logger.info(s"STATUS[$migrationId]: $status")
      if (status != Completed.name && status != Failed.name && status != Canceled.name) {
        sleep()
        trackMigrationJob(migrationId, spaceId, projectId)
      } else {
        if ( status == Completed.name ) {
          logger.info(s"STATUS[$migrationId]: $status|${doc.asJsObject.fields("results")}")
        } else {
          logger.info(s"STATUS[$migrationId]: $status|${doc.asJsObject.fields("console")}")
        }

        Future.successful(doc)
      }
    })
  }

  override def cleanupMigrationJob(id: String,
                                   spaceId: Option[String],
                                   projectId: Option[String])(implicit ec: ExecutionContext) = {
    for {
      latestDoc <- cloudantClient.find(id)
      _ <- cloudantClient.remove(latestDoc.toJson)
    } yield {}
  }
}
