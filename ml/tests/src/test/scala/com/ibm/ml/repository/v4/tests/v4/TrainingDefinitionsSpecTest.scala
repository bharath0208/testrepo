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

import com.ibm.analytics.cams.api.v2.assets.Asset
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentResource
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionResource
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineResource
import com.ibm.analytics.wml.api.v4ga.training_definitions.TrainingDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.training_definitions.TrainingDefinitionResource
import com.ibm.ml.repository.v4.tests.Sanity
import com.ibm.ml.repository.v4.tests.tags.{SpaceAndProject, TrainingDefinitions}
import org.scalatest.DoNotDiscover
import spray.json.{DefaultJsonProtocol, JsObject, _}
import ResourceLoader._

import scala.language.postfixOps

@DoNotDiscover
@TrainingDefinitions
@SpaceAndProject
class TrainingDefinitionsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {

  //default constructor required for NGPTestReporter
  def this() = this("project", "user", ("", ""))

  private var experimentId: String = _
  private var modelDefId: String = _
  private var pipelineId: String = _
  private var hardwareSpecId: String = _
  private var softwareSpecId: Option[String] = _
  private var dataAssetId: Option[String] = _
  private var trainingDefinition: TrainingDefinitionResource = _

  override def containerType: String = cType

  override def authTokenType: String = tokenType

  override def spaceId = if (cType == "space") Some(containerAndCreator._1) else None

  override def projectId = if (cType == "project") Some(containerAndCreator._1) else None

  override def migrationUserId = containerAndCreator._2

  override def deleteContainer: Boolean = tidyUp

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    experimentId = createExperiment()._1.convertTo[ExperimentResource].metadata.id
    modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
    pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
    hardwareSpecId = getHardwareSpec()
    softwareSpecId = createSoftwareSpec()._2
    dataAssetId = createDataAsset()._1.convertTo[Asset].metadata.assetId
    trainingDefinition = createTrainingDefinition(
      softwareSpecId=softwareSpecId,
      hardwareSpecId=Some(hardwareSpecId),
      modelDefId=Some(modelDefId),
      pipelineId = Some(pipelineId),
      experimentId = Some(experimentId),
      dataAssetId = dataAssetId
    )._1.convertTo[TrainingDefinitionResource]
    // give DB some time to sync up
    sleep(3000)
  }

  "checking TrainingDefinitions API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::TDEF-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val res = createTrainingDefinition(
          fileName ="create_request_experiment.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId
        )
        assert(res._3 == 201)
        val resource = res._1.convertTo[TrainingDefinitionResource]
        assert(resource.entity.experiment.isDefined, "Failed to get the experiment details")
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-2:return 400-ReservedValue for invalid experimentId" in {
        val res = createTrainingDefinition(
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some("invalidId"),
          dataAssetId = dataAssetId,
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-3:return 400-ReservedValue for invalid modelDefId" in {
        val res = createTrainingDefinition(
          fileName ="create_request_model_def.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some("Invalid"),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-4:return 400-ReservedValue for invalid pipelineId" in {
        val res = createTrainingDefinition(
          fileName ="create_request_pipeline.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some("invalidId"),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-5:return 404-invalid_request_entity for invalid softwareSpec Id" in {
        val res = createTrainingDefinition(
          fileName ="create_request_model_def.json",
          softwareSpecId=Some("InvalidId"),
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(404)
        )
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-6:return 404-invalid_request_entity for invalid hardwareSpec id" in {
        val res = createTrainingDefinition(
          fileName ="create_request_model_def.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some("invalidId"),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(404)
        )
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-7:return 400-ReservedValue for invalid dataAssetId" in {
        val res = createTrainingDefinition(
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = Some("invalidId"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-8:return 400-invalid_request_entity for invalid name" in {
        val res = createTrainingDefinition(name = Some(""),
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-9:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createTrainingDefinition(spaceId = Some(""),
          projectId = None,
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-10:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createTrainingDefinition(spaceId = Some("InvalidId"),
          projectId = None,
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-11:return 201 success with pipeline type".taggedAs(Sanity) in {
        val res = createTrainingDefinition(
          fileName ="create_request_pipeline.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId
        )
        assert(res._3 == 201)
        val resource = res._1.convertTo[TrainingDefinitionResource]
        assert(resource.entity.pipeline.isDefined, "Failed to get the pipeline details")
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-12:return 201 success with type model def".taggedAs(Sanity) in {
        val res = createTrainingDefinition(
          fileName ="create_request_model_def.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId
        )
        assert(res._3 == 201)
        val resource = res._1.convertTo[TrainingDefinitionResource]
        assert(resource.entity.modelDefinition.isDefined, "Failed to get the model def details")
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-13:return 201 success with type fl".taggedAs(Sanity) in {
        val res = createTrainingDefinition(
          fileName ="create_request_fl.json",
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          modelDefId=Some(modelDefId),
          pipelineId = Some(pipelineId),
          experimentId = Some(experimentId),
          dataAssetId = dataAssetId
        )
        assert(res._3 == 201)
        val resource = res._1.convertTo[TrainingDefinitionResource]
        assert(resource.entity.federatedLearning.isDefined, "Failed to get the federated learning details")
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::TDEF-GET-1:return 200".taggedAs(Sanity) in {
        val res = client.trainingDefinitions.getById(id = trainingDefinition.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-training-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::TDEF-GET-2:return 400-invalid_parameter for invalid training definition id" in {
        val res = client.trainingDefinitions.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-training-definition"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.trainingDefinitions.getByIdAsJson(id = trainingDefinition.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-training-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::TDEF-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.trainingDefinitions.get(spaceId = spaceId, projectId = projectId, requestId = Some("getAll-trainingDefinitions"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::TDEF-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.trainingDefinitions.getAsJson(
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("getAll-trainingDefinitions"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.trainingDefinitions.getAsJson(
          spaceId = Some(""),
          projectId = None,
          requestId = Some("getAll-trainingDefinitions"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::TDEF-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.trainingDefinitions.update(
          id = trainingDefinition.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-trainingDefinitions-1"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.get == "newName")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::TDEF-UPDATE-2:return 400-invalid_parameter for invalid trainingDefinition id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.trainingDefinitions.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-trainingDefinition-2"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.trainingDefinitions.updateRaw(
          id = trainingDefinition.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("update-trainingDefinitions-3"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.trainingDefinitions.updateRaw(
          id = trainingDefinition.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-trainingDefinitions-4"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-UPDATE-5:return 400-invalid_patch" in {
        val patchPayload =
          s"""[{"op":"replace","path":"/name"}]""".stripMargin.parseJson
        val res = client.trainingDefinitions.updateRaw(
          id = trainingDefinition.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-trainingDefinitions-5"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("unable_to_perform" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-UPDATE-6:return 400-invalid_patch" in {
        val patchPayload =
          s"""[{"op":"","path":"/description","value":"SVT pipeline - updated"}]""".stripMargin.parseJson
        val res = client.trainingDefinitions.updateRaw(
          id = trainingDefinition.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-trainingDefinitions-6"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("request_body_error" == getErrorCodeFromResponse(res._1))
      }
    }

    "create revision" should {
      s"$containerType|$authTokenType token::TDEF-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.trainingDefinitions.createRevision(
          id = trainingDefinition.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-trainingDefinition-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-REV-2:return 400-invalid_parameter for invalid trainingDefinition id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.trainingDefinitions.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-trainingDefinition-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::TDEF-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.trainingDefinitions.createRevisionRaw(
          id = trainingDefinition.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-trainingDefinition-revision"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::TDEF-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.trainingDefinitions.getRevisions(
          id = Some(trainingDefinition.metadata.id),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("getAll-trainingDefinition-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::TDEF-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.trainingDefinitions.createRevision(
            id = trainingDefinition.metadata.id,
            payload = createRevisionPayload,
            requestId = Some("create-trainingDefinitions-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.trainingDefinitions.getRevisions(
            id = Some(trainingDefinition.metadata.id),
            spaceId = spaceId,
            projectId = projectId,
            limit = Some(i-1),
            requestId = Some("getAll-trainingDefinition-revisions"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }
  }

}
