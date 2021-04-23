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

import akka.actor.ActorSystem
import com.ibm.analytics.cams.api.v2.assets.Asset
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
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
import com.ibm.ml.repository.v4.migration.cloudant.SimpleCloudantClient
import com.ibm.ml.repository.v4.migration.models.MigrationDoc
import com.ibm.ml.repository.v4.migration.upgrade
import com.ibm.ml.repository.v4.migration.upgrade.api.v4.driver.{DBTestDriver, MigrationTestDriver}
import com.ibm.ml.repository.v4.migration.utils.TestCloudantClient
import com.ibm.ml.repository.v4.migration.utils.v4beta.repository.V4BetaTestServicesClient
import com.ibm.ml.repository.v4.tests.v4.{Container, MLRepoTestBase}
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
  implicit var ec: ExecutionContext = ActorSystem().dispatcher
  protected val MAX_ALLOWED_MIGRATION_DURATION = "180s"
  private var migrationIDsCreated: Seq[String] = Seq()
  private var spacesCreated: Seq[String] = Seq()
  private var projectsCreated: Seq[String] = Seq()

  //Used to test CPD migration. Created directly on cloudant so that libraries and runtimes (which are also created directly on cloudant)
  //can refer to this spaceId. This is required because when migration tests are run against a CPD 3.5 cluster, v4 spaces API is not
  //available on the cluster.
  protected var v4SpaceId: Option[String] = _

  //The space/project to which assets are migrated in a CPD cluster
  protected var newSpaceId: Option[String] = _
  protected var newProjectId: Option[String] = _
  private var cloudantRecordsCreated: Seq[String] = Seq()

  private var v4BetaAssets: Map[String, Seq[String]] = Map()
  protected var testDriver: MigrationTestDriver = _

  val (dbUrl, dbUsername, dbPassword, dbName) = SimpleCloudantClient.getDBConfig(config)
  val cloudantClient = TestCloudantClient(dbUrl, "mlrepository-db", dbUsername, dbPassword, None, true)(ActorSystem())

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    if(deploymentType == "ICP" && containerType == "space"){
      cloudantClient.save(loadFromFile(fileName = "upgrade.v4betapayloads/spaces/create_space.json", spaceId = spaceId)).map(spaceRes => {
        import DefaultJsonProtocol._
        v4SpaceId = Some(spaceRes.asJsObject.fields("_id").convertTo[String])
        assert(v4SpaceId == spaceId)
        cloudantRecordsCreated ++ Seq(v4SpaceId)
      })
    }
    hardwareSpecifications = v4BetaClient.hardwareSpecifications.get(expectedStatus = Seq(200))._1
    softwareSpecifications = v4BetaClient.softwareSpecifications.get(expectedStatus = Seq(200))._1
    testDriver = DBTestDriver(migrationUserId = migrationUserId, oldInstanceId = oldInstanceId)
  }

  override protected def afterAll(): Unit = {
    migrationIDsCreated.map(id => Await.result(testDriver.cleanupMigrationJob(id, spaceId, projectId), Duration.create(MAX_ALLOWED_MIGRATION_DURATION)))
    cloudantRecordsCreated.map(cloudantClient.find(_).map(latestRec => cloudantClient.remove(latestRec)))

    v4BetaAssets.foreach(assetType => assetType._1 match {
      case "experiments" => assetType._2.foreach(id => v4BetaClient.experiments.delete(id, spaceId = spaceId, projectId = projectId))
      case "functions" => assetType._2.foreach(id => v4BetaClient.functions.delete(id, spaceId = spaceId, projectId = projectId))
      case "models" => assetType._2.foreach(id => v4BetaClient.models.delete(id, spaceId = spaceId, projectId = projectId))
      case "pipelines" => assetType._2.foreach(id => v4BetaClient.pipelines.delete(id = id, spaceId = spaceId, projectId = projectId))
      case "libraries" => assetType._2.foreach(id => v4BetaClient.library.delete(id))
      case _ => info(s"Failed to delete unknown asset type ${assetType._1}")
    })

    super.afterAll()
    spacesCreated.map(id => client.spacesv2.delete(id = id, requestId = Some("delete-space"), expectedStatus = Some(202)))
    projectsCreated.map(id => client.projectsTransactional.delete(id = id, requestId = Some("delete-project")))
  }

  override def createClient(): Unit = {
    super.createClient()
    if (containerType == "ICP") {
      val newContainer = createNewContainer()
      newSpaceId = newContainer._1
      newProjectId = newContainer._2
    }
    val config: Config = ConfigFactory.load()
    oldInstanceId = sys.env.getOrElse("TEST_WML_OLD_INSTANCE_ID", fail("required env var 'TEST_WML_OLD_INSTANCE_ID' not found!"))
    v4BetaClient = V4BetaTestServicesClient.getV4Client(config = config, containerType = containerType, instanceId = oldInstanceId)
  }

  protected def createNewContainer(): (Option[String], Option[String]) = {
    if (containerType == "space") {
      val newSpaceId = Container.getSpace(authTokenType)._1
      spacesCreated = spacesCreated ++ Seq(newSpaceId)
      (Some(newSpaceId), None)
    } else {
      val newProjectId = Container.getProject(authTokenType)._1
      projectsCreated = projectsCreated ++ Seq(newProjectId)
      (None, Some(newProjectId))
    }
  }

  protected def convertToMigrationDoc(migrationDoc: JsValue) = {
    if (deploymentType == "ICP") {
      import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
      migrationDoc.convertTo[MigrationDoc]
    } else {
      import com.ibm.ml.repository.v4.migration.upgrade.models.MigrationJsonFormat._
      migrationDoc.convertTo[upgrade.models.MigrationDoc]
    }
  }

  protected def startMigration(experimentIds: Option[Seq[String]] = None,
                                   functionIds: Option[Seq[String]] = None,
                                   pipelineIds: Option[Seq[String]] = None,
                                   modelIds: Option[Seq[String]] = None,
                                   modelDefinitionIds: Option[Seq[String]] = None,
                                   deploymentJobDefinitionIds: Option[Seq[String]] = None,
                                   newSpaceId: Option[String] = None,
                                   newProjectId: Option[String] = None,
                                   upgrade: Boolean = false): Future[JsValue] = {
    if (deploymentType == "ICP") {
      startMigrationUpgrade(experimentIds, functionIds, pipelineIds, modelIds, modelDefinitionIds, deploymentJobDefinitionIds, newSpaceId, newProjectId)
    } else {
      startCloudMigration(experimentIds, functionIds, pipelineIds, modelIds)
    }
  }

  private def startCloudMigration(experimentIds: Option[Seq[String]], functionIds: Option[Seq[String]], pipelineIds: Option[Seq[String]], modelIds: Option[Seq[String]]) = {
    for {
      mDoc <- testDriver.startMigration(experimentIds, functionIds, pipelineIds, modelIds, spaceId, projectId)
    } yield {
      info(s"Created migrationJob with id: ${mDoc.id}")
      migrationIDsCreated = migrationIDsCreated ++ Seq(mDoc.id.getOrElse(fail("Migration job created but ID not available in response")))
      import com.ibm.ml.repository.v4.migration.models.MigrationJsonFormat._
      mDoc.toJson
    }
  }

  protected def startMigrationUpgrade(experimentIds: Option[Seq[String]] = None,
                                      functionIds: Option[Seq[String]] = None,
                                      pipelineIds: Option[Seq[String]] = None,
                                      modelIds: Option[Seq[String]] = None,
                                      modelDefinitionIds: Option[Seq[String]] = None,
                                      deploymentJobDefinitionIds: Option[Seq[String]] = None,
                                      newSpaceId: Option[String] = None,
                                      newProjectId: Option[String] = None): Future[JsValue] = {

    for {
      mDoc <- testDriver.startMigrationUpgrade(experimentIds, functionIds, pipelineIds, modelIds, modelDefinitionIds, deploymentJobDefinitionIds, spaceId, projectId, newSpaceId, newProjectId)
    } yield {
      info(s"Created migrationJob with id: ${mDoc.id}")
      migrationIDsCreated = migrationIDsCreated ++ Seq(mDoc.id.getOrElse(fail("Migration job created but ID not available in response")))
      import com.ibm.ml.repository.v4.migration.upgrade.models.MigrationJsonFormat._
      mDoc.toJson
    }
  }

  protected def trackMigrationJob(migrationId: String) = {
    testDriver.trackMigrationJob(migrationId, spaceId, projectId)
  }

  protected def futureSleep(msecs: Long = 100): Future[Unit] = {
    Future.fromTry(Try(Thread.sleep(msecs)))
  }

  protected def createV4BetaLibrary(fileName: String = "upgrade.v4betapayloads/libraries/create_library.json",
                                    spaceId: Option[String] = None,
                                    projectId: Option[String] = None): String = {
    if (deploymentType == "ICP" && containerType == "space") {
      import DefaultJsonProtocol._
      val entity = loadFromFile(fileName = "upgrade.v4betapayloads/libraries/create_library_cloudant.json", spaceId = spaceId, projectId = projectId)
      val library = Await.result(cloudantClient.save(entity), Duration.create(MAX_ALLOWED_MIGRATION_DURATION))
      val libraryId = library.asJsObject.fields("_id").convertTo[String]
      cloudantRecordsCreated ++ Seq(libraryId)
      libraryId
    } else {
      val entity = loadFromFile(fileName = fileName, spaceId = spaceId, projectId = projectId)
      val res = v4BetaClient.library.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[LibraryResource]
      v4BetaAssets = v4BetaAssets ++ Map("libraries" -> v4BetaAssets.getOrElse("libraries", Seq()).appended(res.metadata.id))
      res.metadata.id
    }
  }

  protected def createV4BetaPipeline(fileName: String = "upgrade.v4betapayloads/pipelines/create_pipeline_1.json",
                                     libraryId: Option[String] = None,
                                     modelDefinitionId: Option[String] = None,
                                     spaceId: Option[String] = None,
                                     projectId: Option[String] = None): PipelineResource = {
    val entity = loadFromFile(fileName = fileName, libraryId = libraryId, modelDefinitionId = modelDefinitionId, spaceId = spaceId, projectId = projectId)
    val res = v4BetaClient.pipelines.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[PipelineResource]
    v4BetaAssets = v4BetaAssets ++ Map("pipelines" -> v4BetaAssets.getOrElse("pipelines", Seq()).appended(res.metadata.id))
    res
  }

  protected def createV4BetaExperiment(fileName: String = "upgrade.v4betapayloads/experiments/create_experiment.json",
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

  protected def createV4BetaFunction(fileName: String = "upgrade.v4betapayloads/functions/create_function.json",
                                     runtimeHref: Option[String] = Some("/v4/runtimes/tensorflow_1.2-py3"),
                                     spaceId: Option[String] = None,
                                     projectId: Option[String] = None): FunctionResource = {
    val entity = loadFromFile(fileName = fileName, runtimeHref = runtimeHref, spaceId = spaceId, projectId = projectId)
    val res = v4BetaClient.functions.createJson(entity = Some(entity), expectedStatus = Seq(201))._1.convertTo[FunctionResource]
    sleep()
    v4BetaClient.functions.upload(id = res.metadata.id, spaceId = spaceId, projectId = projectId,
      contents = "sample code".getBytes(), contentFormat = Some("text/plain"), expectedStatus = Some(200))
    v4BetaAssets = v4BetaAssets ++ Map("functions" -> v4BetaAssets.getOrElse("functions", Seq()).appended(res.metadata.id))
    res
  }

  protected def createV4BetaModel(fileName: String = "upgrade.v4betapayloads/models/create_model.json",
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

  protected def createV4BetaModelDefinition(fileName: String = "upgrade.v4betapayloads/model_definitions/create_model_definition.json",
                                     spaceId: Option[String] = None,
                                     projectId: Option[String] = None): Asset = {
    val entity = loadFromFile(fileName = fileName, spaceId = spaceId, projectId = projectId)
    val res = v4BetaClient.camsAssets.createJson(entity = Some(entity), spaceId = spaceId, projectId = projectId, expectedStatus = Seq(201))
    val mdef = res._1.convertTo[Asset]
    assert(mdef.metadata.assetId.isDefined)

    sleep()
    import DefaultJsonProtocol._
    v4BetaClient.camsAssets.upload(id = mdef.metadata.assetId.get, spaceId = spaceId, projectId = projectId,
      contents = s"""{"asset_type": "wml_model_definition"}""".toJson.convertTo[String].getBytes(), contentFormat = None, expectedStatus = Some(201))

    v4BetaAssets = v4BetaAssets ++ Map("model_definitions" -> v4BetaAssets.getOrElse("model_definitions", Seq()).appended(mdef.metadata.assetId.get))
    mdef
  }

  protected def createV4BetaDeploymentJobDefinition(fileName: String = "upgrade.v4betapayloads/deployment_job_definitions/create_deployment_job_definition.json",
                                            spaceId: Option[String] = None,
                                            projectId: Option[String] = None): Asset = {
    val entity = loadFromFile(fileName = fileName, spaceId = spaceId, projectId = projectId)
    val res = v4BetaClient.camsAssets.createJson(entity = Some(entity), spaceId = spaceId, projectId = projectId, expectedStatus = Seq(201))
    val djd = res._1.convertTo[Asset]
    assert(djd.metadata.assetId.isDefined)
    v4BetaAssets = v4BetaAssets ++ Map("deployment_job_definitions" -> v4BetaAssets.getOrElse("deployment_job_definitions", Seq()).appended(djd.metadata.assetId.get))
    djd
  }

  protected def getMigratedFrom(jsObj: Option[JsObject]): (String, String, String) = {
    import DefaultJsonProtocol._
    if(jsObj.isDefined){
      val migratedFromObj = jsObj.get.fields.getOrElse("migrated_from", fail("v4ga asset does not contain 'migrated_from' field")).asJsObject
      val assetId = migratedFromObj.fields.getOrElse("asset_id", fail("'migrated_from' does not contain 'asset_id'")).convertTo[String]
      val assetType = migratedFromObj.fields.getOrElse("asset_type", "".toJson).convertTo[String]
      val instanceId = migratedFromObj.fields.getOrElse("instance_id", "".toJson).convertTo[String]
      (assetId, assetType, instanceId)
    } else {
      ("", "", "")
    }
  }

}
