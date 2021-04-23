/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.v4

import com.ibm.analytics.wml.utils.containers._
import com.ibm.ml.repository.v4.tests.utils.V4TestServicesClient
import com.ibm.ml.repository.v4.tests.v4.ResourceLoader._
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import org.apache.http.Header
import org.scalatest.BeforeAndAfterAll
import org.scalatest.wordspec.AnyWordSpec
import spray.json._

import scala.language.postfixOps
import scala.util.Try

trait MLRepoTestBase extends AnyWordSpec with BeforeAndAfterAll with StrictLogging {
  def containerType: String
  def authTokenType: String
  def spaceId: Option[String]
  def projectId: Option[String]
  def migrationUserId: String
  def deleteContainer: Boolean = true

  protected val invalidSpaceIdError = 404
  protected val invalidSpaceIdCode = "invalid_query_parameter"
  protected val invalidSpaceIdInRequestBodyCode = "invalid_request_entity"
  protected val emptySpaceIdError = 400
  protected val emptySpaceIdQuerySpaceCode = "missing_query_parameter"
  protected val emptySpaceIdQueryCode = "missing_one_of_query_parameter"
  protected val emptySpaceIdEntityCode = "invalid_request_entity"
  protected val invalidJSONError = 400
  protected val invalidJSONCode = "malformed_json"

  protected var client: V4TestServicesClient = _
  protected var deploymentType: String = _
  protected val config: Config = ConfigFactory.load()

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    createClient()
    // give DB some time to sync up
    sleep(3000)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    if (deleteContainer) {
      Container.deleteSpace
      Container.deleteProject
    }
  }

  protected def createClient(): Unit = {
    client = V4TestServicesClient.getV4Client(config = config, containerType = containerType, authTokenType = authTokenType)
    info(s"**** USING CONTAINER:: containerType: $containerType, spaceId: $spaceId, projectId: $projectId, creatorId: $migrationUserId ****")
    logger.info(s"**** USING CONTAINER:: containerType: $containerType, spaceId: $spaceId, projectId: $projectId, creatorId: $migrationUserId ****")
  }

  protected def getErrorCodeFromResponse(res: JsValue): String = {
    import DefaultJsonProtocol._
    val errorCodeJsVal = res.asJsObject().fields.get("errors").map(_.convertTo[List[JsObject]]).map(_.head.fields("code")).get
    errorCodeJsVal.convertTo[String]
  }

  protected def createExperiment(fileName: String="create_request.json",
                                 name: Option[String] = Some("testExperiment"),
                                 spaceId: Option[String] = spaceId,
                                 projectId: Option[String] = projectId,
                                 pipelineId: Option[String] = None,
                                 modelDefId: Option[String] = None,
                                 softwareSpecId: Option[String] = None,
                                 hardwareSpecId: Option[String] = None,
                                 softwareSpecName: Option[String] = None,
                                 hardwareSpecName: Option[String] = None,
                                 expectedStatus: Option[Int] = None): (JsValue, Array[Header], Int) = {
    val experimentEntity = loadFromFile(fileName=s"experiments/$fileName",
      spaceId=spaceId,
      projectId=projectId,
      name=name,
      pipelineId=pipelineId,
      modelDefinitionId=modelDefId,
      swSpecId = softwareSpecId,
      hwSpecId = hardwareSpecId,
      swSpecName = softwareSpecName,
      hwSpecName = hardwareSpecName)
    client.experiments.createJson(entity = Some(experimentEntity), spaceId = spaceId, projectId = projectId, requestId = Some("create-experiment"), expectedStatus = expectedStatus)
  }

  protected def createFunction(fileName: String="create_request.json",
                               name: Option[String] = Some("testFunction"),
                               spaceId: Option[String] = spaceId,
                               projectId: Option[String] = projectId,
                               swSpecId: Option[String] = None,
                               expectedStatus: Option[Int] = None): (JsValue, Array[Header], Int) = {
    assert(name.isDefined)
    assert(swSpecId.isDefined)
    val functionEntity = loadFromFile(fileName=s"functions/$fileName", spaceId=spaceId, projectId = projectId, swSpecId = swSpecId, name=name)
    client.functions.createJson(entity = Some(functionEntity), spaceId = spaceId, projectId = projectId, requestId = Some("create-function"), expectedStatus = expectedStatus)
  }

  protected def createModelDefinition(fileName: String="create_request.json",
                                      name: String="testModelDefinition",
                                      spaceId: Option[String] = spaceId,
                                      projectId: Option[String] = projectId,
                                      version: String="v",
                                      platformName: String="pName",
                                      expectedStatus: Option[Int] = None): (JsValue, Array[Header], Int) = {
    val entity = loadFromFile(fileName=s"model_definitions/$fileName",
      spaceId=spaceId,
      projectId=projectId,
      name=Some(name),
      modelVersion=Some(version),
      platformName=Some(platformName)
    )
    client.modelDefinitions.createJson(entity = Some(entity), spaceId=spaceId, projectId=projectId, requestId=Some("create-model-definition"), expectedStatus=expectedStatus)
  }

  protected def createPipeline(fileName: String="create_request.json",
                               name: Option[String] = Some("testPipeline"),
                               spaceId: Option[String] = spaceId,
                               projectId: Option[String] = projectId,
                               softwareSpecId: Option[String] = None,
                               hardwareSpecId: Option[String] = None,
                               modelDefId: Option[String] = None,
                               expectedStatus: Option[Int] = None): (JsValue, Array[Header], Int) = {
    val pipelineEntity = loadFromFile(fileName=s"pipelines/$fileName",
      name = name,
      spaceId=spaceId,
      projectId=projectId,
      swSpecId = softwareSpecId,
      hwSpecId = hardwareSpecId,
      modelDefinitionId = modelDefId)
    client.pipelines.createJson(entity = Some(pipelineEntity), spaceId = spaceId, projectId = projectId, requestId = Some("create-pipeline"), expectedStatus = expectedStatus)
  }

  protected def createModel(fileName: String="create_request.json",
                            name: String="testModel",
                            spaceId: Option[String] = spaceId,
                            projectId: Option[String] = projectId,
                            softwareSpecId: Option[String],
                            modelDefId: String,
                            pipelineId: Option[String] = None,
                            expectedStatus: Option[Int] = Some(201)): (JsValue, Array[Header], Int) = {
    val entity = loadFromFile(fileName=s"models/$fileName",
      spaceId=spaceId,
      projectId=projectId,
      name=Some(name),
      swSpecId=softwareSpecId,
      modelDefinitionId=Some(modelDefId),
      pipelineId = pipelineId
    )
    client.models.createJson(entity = Some(entity), spaceId=spaceId, projectId=projectId, requestId=Some("create-model"), expectedStatus=expectedStatus)
  }

  protected def createSoftwareSpec(fileName: String="create_request.json",
                                   spaceId: Option[String] = spaceId,
                                   projectId: Option[String] = projectId): (Option[String], Option[String]) ={
    val availableSwSpecs = client.softwareSpecifications.get(spaceId = spaceId, projectId = projectId, expectedStatus = Some(200))
    val baseSoftwareSpecificationId = availableSwSpecs._1.resources.find(_.metadata.name.get == "default_py3.6").map(_.metadata.assetId)
    val swSpecCreateResponse = baseSoftwareSpecificationId.map(baseSwSpecId => {
      val swSpecCreateRequest = loadFromFile(s"software_spec/$fileName", baseSwSpecId = Some(baseSwSpecId))
      client.softwareSpecifications.createJson(
        spaceId = spaceId,
        projectId = projectId,
        entity = Some(swSpecCreateRequest),
        expectedStatus = Some(201),
        checkResponse = false)
    })

    val swSpecId = for {
      swSpecRes <- swSpecCreateResponse
      metadata <- swSpecRes._1.asJsObject.fields.get("metadata")
      assetId <- metadata.asJsObject.fields.get("asset_id")
    } yield {
      import DefaultJsonProtocol._
      assetId.convertTo[String]
    }


    (baseSoftwareSpecificationId, swSpecId)
  }

  protected def getContainer(spaceId: Option[String],
                             projectId: Option[String]): Container = {
    if (spaceId.isDefined)
      Space(spaceId.get)
    else if (projectId.isDefined)
      Project(projectId.get)
    else
      fail("no space or project id")
  }

  protected def getContainerName(spaceId: Option[String],
                                 projectId: Option[String]): String = {
    getContainer(spaceId, projectId).toString
  }

  protected def getHardwareSpec(name: String="XXS",
                                spaceId: Option[String] = spaceId,
                                projectId: Option[String] = projectId): String = {
    val hwSpecs = client.hardwareSpecifications.get(spaceId = spaceId, projectId = projectId, expectedStatus = Some(200))
    hwSpecs._1.resources.find(_.metadata.name.get == name).getOrElse {
      import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecJsonFormat._
      fail(s"Could not get hardware spec $name for ${getContainerName(spaceId, projectId)} from response ${logPrint(hwSpecs._1.resources.toJson)}")
    }.metadata.assetId
  }

  protected def createDataAsset(fileName: String="create_request.json",
                                name: String="testDataAsset",
                                spaceId: Option[String] = spaceId,
                                projectId: Option[String] = projectId,
                                softwareSpecId: Option[String] = None,
                                modelDefId: Option[String] = None,
                                pipelineId: Option[String] = None,
                                expectedStatus: Option[Int] = Some(201)): (JsValue, Array[Header], Int) = {
    val entity = loadFromFile(fileName=s"data_assets/$fileName",
      spaceId=spaceId,
      projectId=projectId
    )
    client.dataAssets.createJson(entity = Some(entity), spaceId=spaceId, projectId=projectId, requestId=Some("create-data-asset"), expectedStatus=expectedStatus)
  }

  protected def createTrainingDefinition(fileName: String="create_request_experiment.json",
                                         name: Option[String] = Some("testTrainingDefinition"),
                                         spaceId: Option[String] = spaceId,
                                         projectId: Option[String] = projectId,
                                         softwareSpecId: Option[String] = None,
                                         hardwareSpecId: Option[String] = None,
                                         modelDefId: Option[String] = None,
                                         pipelineId: Option[String] = None,
                                         experimentId: Option[String] = None,
                                         dataAssetId: Option[String] = None,
                                         federatedLearning: Option[JsValue] = None,
                                         expectedStatus: Option[Int] = Some(201)): (JsValue, Array[Header], Int) = {
    val trainingDefinitionEntity: JsValue = loadFromFile(fileName=s"training_definitions/$fileName",
      name = name,
      spaceId = spaceId,
      projectId = projectId,
      swSpecId = softwareSpecId,
      hwSpecId = hardwareSpecId,
      modelDefinitionId = modelDefId,
      pipelineId = pipelineId,
      experimentId = experimentId,
      dataAssetId = dataAssetId)

    val entity: JsValue = federatedLearning match {
      case Some(fl) =>
        JsObject(trainingDefinitionEntity.asJsObject.fields ++ Map("federated_learning" -> fl))
      case None =>
        trainingDefinitionEntity
    }

    client.trainingDefinitions.createJson(entity = Some(entity),
      spaceId = spaceId,
      projectId = projectId,
      requestId = Some("create-training-definition"),
      expectedStatus = expectedStatus)
  }

  protected def createRemoteTrainingSystem(fileName: String="create_request.json",
                                           name: Option[String] = Some("test remote training system"),
                                           spaceId: Option[String] = spaceId,
                                           projectId: Option[String] = projectId,
                                           expectedStatus: Option[Int] = Some(201)): (JsValue, Array[Header], Int) = {
    val remoteTrainingSystemEntity = loadFromFile(fileName=s"remote_training_systems/$fileName",
      name = name,
      spaceId=spaceId,
      projectId=projectId)

    client.remoteTrainingSystems.createJson(entity = Some(remoteTrainingSystemEntity),
      spaceId = spaceId,
      projectId = projectId,
      requestId = Some("create-remote-training-system"),
      expectedStatus = expectedStatus)
  }


  protected def createDeploymentJobDefinition(fileName: String="create_request.json",
                                              name: Option[String] = Some("testDeploymentJobDefinition"),
                                              spaceId: Option[String] = spaceId,
                                              hardwareSpecId: Option[String] = None,
                                              dataAssetId: Option[String] = None,
                                              expectedStatus: Option[Int] = Some(201)): (JsValue, Array[Header], Int) = {
    val deploymentJobDefinitionEntity = loadFromFile(fileName=s"deployment_job_definitions/$fileName",
      name = name,
      spaceId=spaceId,
      hwSpecId = hardwareSpecId,
      dataAssetId = dataAssetId)

    client.deploymentJobDefinitions.createJson(entity = Some(deploymentJobDefinitionEntity),
      spaceId = spaceId,
      requestId = Some("create-deployment-job-definition"),
      expectedStatus = expectedStatus)
  }

  protected def logPrint(js: JsValue): String = {
    js.prettyPrint
  }

  protected def sleep(msecs: Long = 100): Unit = {
    Try(Thread.sleep(msecs))
  }

  protected def sleepAfterDelete(): Unit = {
    sleep(400)
  }
}
