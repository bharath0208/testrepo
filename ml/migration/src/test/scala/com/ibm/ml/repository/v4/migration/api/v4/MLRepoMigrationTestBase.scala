/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.analytics.wml.repository.ml.migration.api.v4

import akka.actor.ActorSystem
import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecificationResources
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecifications
import com.ibm.analytics.wml.api.v4.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4.experiments.ExperimentResource
import com.ibm.analytics.wml.api.v4.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4.functions.FunctionResource
import com.ibm.analytics.wml.api.v4.libraries.LibraryJsonFormat._
import com.ibm.analytics.wml.api.v4.libraries.LibraryResource
import com.ibm.analytics.wml.api.v4.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4.models.ModelResource
import com.ibm.analytics.wml.api.v4.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4.pipelines.PipelineResource
import com.ibm.ml.repository.v4.migration.api.v4.driver.{DBTestDriver, MigrationTestDriver}
import com.ibm.ml.repository.v4.migration.cloudant.SimpleCloudantClient
import com.ibm.ml.repository.v4.migration.models._
import com.ibm.ml.repository.v4.migration.utils.v4beta.repository.V4BetaTestServicesClient
import com.ibm.ml.repository.v4.tests.v4.MLRepoTestBase
import com.typesafe.config.{Config, ConfigFactory}
import spray.json._
import com.ibm.ml.repository.v4.tests.v4.ResourceLoader._

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.Try

trait MLRepoMigrationTestBase extends MLRepoTestBase {

  protected var v4BetaClient: V4BetaTestServicesClient = _
  protected var hardwareSpecifications: HardwareSpecificationResources = _
  protected var softwareSpecifications: SoftwareSpecifications = _
  protected var oldInstanceId: String = _
  implicit var ec: ExecutionContext = _
  protected val MAX_ALLOWED_MIGRATION_DURATION = "60s"
  private var migrationDocsCreated: Map[String, MigrationDoc] = Map()

  private var v4BetaAssets: Map[String, Seq[String]] = Map()
  protected var testDriver: MigrationTestDriver = _
  private val defaultSystem = ActorSystem()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    ec = defaultSystem.dispatcher
    hardwareSpecifications = v4BetaClient.hardwareSpecifications.get(expectedStatus = Seq(200))._1
    softwareSpecifications = v4BetaClient.softwareSpecifications.get(expectedStatus = Seq(200))._1
    val (dbUrl, dbUsername, dbPassword, dbName) = SimpleCloudantClient.getDBConfig(config)

    val cloudantClient = SimpleCloudantClient(dbUrl, dbName, dbUsername, dbPassword)(defaultSystem)
    testDriver = DBTestDriver(cloudantClient = cloudantClient, migrationUserId = migrationUserId, oldInstanceId = oldInstanceId)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    migrationDocsCreated.map(doc => Await.result(testDriver.cleanupMigrationJob(doc._2, spaceId, projectId), Duration.create(MAX_ALLOWED_MIGRATION_DURATION)))

    v4BetaAssets.foreach(assetType => assetType._1 match {
      case "experiments" => assetType._2.foreach(id => v4BetaClient.experiments.delete(id))
      case "functions" => assetType._2.foreach(id => v4BetaClient.functions.delete(id))
      case "models" => assetType._2.foreach(id => v4BetaClient.models.delete(id))
      case "pipelines" => assetType._2.foreach(id => v4BetaClient.pipelines.delete(id))
      case "libraries" => assetType._2.foreach(id => v4BetaClient.library.delete(id))
      case _ => info(s"Failed to delete unknown asset type ${assetType._1}")
    })
  }

  override def createClient(): Unit = {
    super.createClient()
    val config: Config = ConfigFactory.load()
    oldInstanceId = sys.env.getOrElse("TEST_WML_OLD_INSTANCE_ID", fail("required env var 'TEST_WML_OLD_INSTANCE_ID' not found!"))
    v4BetaClient = V4BetaTestServicesClient.getV4Client(config = config, containerType = containerType, instanceId = oldInstanceId)
  }

  protected def startMigration(experimentIds: Option[Seq[String]] = None,
                                   functionIds: Option[Seq[String]] = None,
                                   pipelineIds: Option[Seq[String]] = None,
                                   modelIds: Option[Seq[String]] = None): Future[MigrationDoc] = {

    for {
      mDoc <- testDriver.startMigration(experimentIds, functionIds, pipelineIds, modelIds, spaceId, projectId)
    } yield {
      info(s"Created migrationJob with id: ${mDoc.id}")
      migrationDocsCreated = migrationDocsCreated ++ Map(mDoc.id.getOrElse(fail("Migration job created but ID not available in response")) -> mDoc)
      mDoc
    }
  }

  protected def trackMigrationJob(migrationId: String): Future[MigrationDoc] = {
    for{
      mDoc <- testDriver.trackMigrationJob(migrationId, spaceId, projectId)
    } yield {
      migrationDocsCreated = migrationDocsCreated ++ Map(mDoc.id.getOrElse(fail("Get Migration job returned do without ID")) -> mDoc)
      mDoc
    }
  }

  protected def cleanupMigrationJob(migrationResult: MigrationDoc) = {
    testDriver.cleanupMigrationJob(migrationResult, spaceId, projectId)
  }

  protected def futureSleep(msecs: Long = 100): Future[Unit] = {
    Future.fromTry(Try(Thread.sleep(msecs)))
  }

  protected def createV4BetaLibrary(fileName: String = "v4betapayloads/libraries/create_library.json"): LibraryResource = {
    val entity = loadFromFile(fileName = fileName)
    val res = v4BetaClient.library.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[LibraryResource]
    v4BetaAssets = v4BetaAssets ++ Map("libraries" -> v4BetaAssets.getOrElse("libraries", Seq()).appended(res.metadata.id))
    res
  }

  protected def createV4BetaPipeline(fileName: String = "v4betapayloads/pipelines/create_pipeline_1.json",
                                     libraryId: Option[String] = None): PipelineResource = {
    val entity = loadFromFile(fileName = fileName, libraryId = libraryId)
    val res = v4BetaClient.pipelines.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[PipelineResource]
    v4BetaAssets = v4BetaAssets ++ Map("pipelines" -> v4BetaAssets.getOrElse("pipelines", Seq()).appended(res.metadata.id))
    res
  }

  protected def createV4BetaExperiment(fileName: String = "v4betapayloads/experiments/create_experiment.json",
                                       pipelineHref: Option[String] = None,
                                       libraryHref: Option[String] = None,
                                       libraryId: Option[String] = None,
                                       spaceId: Option[String] = None,
                                       projectId: Option[String] = None): String = {
    val entity = loadFromFile(fileName = fileName, pipelineHref = pipelineHref, libraryId = libraryId, libraryHref = libraryHref, spaceId = spaceId, projectId = projectId)
    val res = v4BetaClient.experiments.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[ExperimentResource]
    v4BetaAssets = v4BetaAssets ++ Map("experiments" -> v4BetaAssets.getOrElse("experiments", Seq()).appended(res.metadata.id))
    res.metadata.id
  }

  protected def createV4BetaFunction(fileName: String = "v4betapayloads/functions/create_function.json",
                                     runtimeHref: Option[String] = Some("/v4/runtimes/tensorflow_1.2-py3"),
                                     spaceId: Option[String] = None,
                                     projectId: Option[String] = None): FunctionResource = {
    val entity = loadFromFile(fileName = fileName, runtimeHref = runtimeHref, spaceId = spaceId, projectId = projectId)
    val res = v4BetaClient.functions.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[FunctionResource]
    v4BetaAssets = v4BetaAssets ++ Map("functions" -> v4BetaAssets.getOrElse("functions", Seq()).appended(res.metadata.id))
    res
  }

  protected def createV4BetaModel(fileName: String = "v4betapayloads/models/create_model.json",
                                     runtimeHref: Option[String] = Some("/v4/runtimes/tensorflow_1.2-py3"),
                                     pipelineHref: Option[String] = None,
                                     trainingLibHref: Option[String] = None,
                                     swSpecId: Option[String] = None,
                                     spaceId: Option[String] = None,
                                     projectId: Option[String] = None): ModelResource = {
    val entity = loadFromFile(fileName = fileName,
      runtimeHref = runtimeHref,
      pipelineHref = pipelineHref,
      trainingLibHref = trainingLibHref,
      swSpecId = swSpecId,
      spaceId = spaceId,
      projectId = projectId)
    val res = v4BetaClient.models.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[ModelResource]
    v4BetaAssets = v4BetaAssets ++ Map("models" -> v4BetaAssets.getOrElse("models", Seq()).appended(res.metadata.id))
    res
  }

  protected def getMigratedFrom(jsObj: Option[JsObject]): (String, String, String) = {
    import DefaultJsonProtocol._
    if(jsObj.isDefined){
      val migratedFromObj = jsObj.get.fields.getOrElse("migrated_from", fail("v4ga asset does not contain 'migrated_from' field")).asJsObject
      val assetId = migratedFromObj.fields.getOrElse("asset_id", fail("'migrated_from' does not contain 'asset_id'")).convertTo[String]
      val assetType = migratedFromObj.fields.getOrElse("asset_type", fail("'migrated_from' does not contain 'asset_type'")).convertTo[String]
      val instanceId = migratedFromObj.fields.getOrElse("instance_id", fail("'migrated_from' does not contain 'instance_id'")).convertTo[String]
      (assetId, assetType, instanceId)
    } else {
      ("", "", "")
    }
  }

}
