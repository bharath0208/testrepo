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

import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentJsonFormat._
import com.ibm.analytics.wml.api.v4ga.experiments.ExperimentResource
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionResource
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineResource
import com.ibm.ml.repository.v4.tests.Sanity
import com.ibm.ml.repository.v4.tests.tags.{Experiments, SpaceAndProject}
import org.scalatest.DoNotDiscover
import spray.json.{DefaultJsonProtocol, JsObject, _}
import ResourceLoader._

import scala.language.postfixOps

@DoNotDiscover
@Experiments
@SpaceAndProject
class ExperimentsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {

  private var experiment: ExperimentResource = _

  //default constructor required for NGPTestReporter
  def this() = this("project", "user", ("", ""))

  override def containerType: String = cType

  override def authTokenType: String = tokenType

  override def spaceId = if (cType == "space") Some(containerAndCreator._1) else None

  override def projectId = if (cType == "project") Some(containerAndCreator._1) else None

  override def migrationUserId = containerAndCreator._2

  override def deleteContainer: Boolean = tidyUp

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    experiment = createExperiment()._1.convertTo[ExperimentResource]
    info("ExperimentId created : "+experiment.metadata.id)

    // give DB some time to sync up
    sleep(3000)
  }

  "checking Experiments API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::EXP-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val res = createExperiment()
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::EXP-CREATE-2:return 201 success for request with training references" in {
        val softwareSpecId = createSoftwareSpec()._2
        assert(softwareSpecId.isDefined, "failed to get the software spec id")
        val hardwareSpecId = getHardwareSpec()
        val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createExperiment(
          fileName = "create_request_with_training_references.json",
          pipelineId=Some(pipelineId),
          modelDefId=Some(modelDefId),
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::EXP-CREATE-3:return 201 success for request with training references containing hardware/software spec names" in {
        val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createExperiment(
          fileName = "create_request_with_training_references_hw_sw_spec_name.json",
          pipelineId=Some(pipelineId),
          modelDefId=Some(modelDefId),
          softwareSpecName=Some("default_py3.6"),
          hardwareSpecName=Some("XXS"))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::EXP-CREATE-4:return 400-ReservedValue for request with invalid pipeline id" in {
        val softwareSpecId = createSoftwareSpec()._2
        val hardwareSpecId = getHardwareSpec()
        // val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createExperiment(
          fileName = "create_request_with_training_references.json",
          pipelineId=Some("Invalid"),
          modelDefId=Some(modelDefId),
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-CREATE-5:return 400-ReservedValue for request with invalid model definition id" in {
        val softwareSpecId = createSoftwareSpec()._2
        val hardwareSpecId = getHardwareSpec()
        val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        // val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createExperiment(
          fileName = "create_request_with_training_references.json",
          pipelineId=Some(pipelineId),
          modelDefId=Some("invalidId"),
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some(hardwareSpecId),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-CREATE-6:return 404-invalid_request_entity for request with invalid software_spec id" in {
        val hardwareSpecId = getHardwareSpec()
        val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createExperiment(
          fileName = "create_request_with_training_references.json",
          pipelineId=Some(pipelineId),
          modelDefId=Some(modelDefId),
          softwareSpecId=Some("invalidId"),
          hardwareSpecId=Some(hardwareSpecId),
          expectedStatus = Some(404))
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-CREATE-7:return 404-invalid_request_entity for request with invalid hardware_spec id" in {
        val softwareSpecId = createSoftwareSpec()._2
        val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createExperiment(
          fileName = "create_request_with_training_references.json",
          pipelineId=Some(pipelineId),
          modelDefId=Some(modelDefId),
          softwareSpecId=softwareSpecId,
          hardwareSpecId=Some("InvalidId"),
          expectedStatus = Some(404))
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }


      s"$containerType|$authTokenType token::EXP-CREATE-8:return 400-invalid_request_entity for invalid name" in {
        val res = createExperiment(spaceId = spaceId,  projectId = projectId, name = Some(""), expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-CREATE-9:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createExperiment(
          spaceId = Some(""),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-CREATE-10:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createExperiment(spaceId = Some("invalidId"), expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::EXP-GET-1:return 200".taggedAs(Sanity) in {
        info(s"GetExperiment :: spaceId: $spaceId, projectId: $projectId, experimentId: ${experiment.metadata.id}")
        val res = client.experiments.getById(id = experiment.metadata.id, spaceId = spaceId, projectId = projectId, requestId = Some("get-model-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::EXP-GET-2:return 400-invalid_parameter for invalid model definition id" in {
        val res = client.experiments.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-model-definition"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.experiments.getByIdAsJson(id = experiment.metadata.id,
          spaceId = Some("invalidId"),
          requestId = Some("get-model-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }


    "getAll" should {
      s"$containerType|$authTokenType token::EXP-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.experiments.get(spaceId = spaceId, projectId = projectId, requestId = Some("getAll-model-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::EXP-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.experiments.getAsJson(
          spaceId = Some("invalidId"),
          requestId = Some("getAll-model-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.experiments.getAsJson(
          spaceId = Some(""),
          requestId = Some("getAll-model-definition"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::EXP-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.experiments.update(
          id = experiment.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-experiment-definition"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.get == "newName")
        assert(res._1.entity.custom.get.fields.contains("testKey"), s"Failed to find 'testKey' in ${res._1.entity.toJson.prettyPrint}")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::EXP-UPDATE-2:return 400-invalid_parameter for invalid experiment id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.experiments.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model-definition"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.experiments.updateRaw(
          id = experiment.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          requestId = Some("update-model-definition"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.experiments.updateRaw(
          id = experiment.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model-definition"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }
    }

    "create revision" should {
      s"$containerType|$authTokenType token::EXP-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.experiments.createRevision(
          id = experiment.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::EXP-CREATE-REV-2:return 400-invalid_parameter for invalid experiment id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.experiments.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::EXP-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.experiments.createRevisionRaw(
          id = experiment.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::EXP-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.experiments.getRevisions(
          id = Some(experiment.metadata.id),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("getAll-model-definition-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::EXP-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.experiments.createRevision(
            id = experiment.metadata.id,
            payload = createRevisionPayload,
            requestId = Some("create-experiment-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.experiments.getRevisions(
            id = Some(experiment.metadata.id),
            spaceId = spaceId,
            projectId = projectId,
            limit = Some(i-1),
            requestId = Some("getAll-experiment-revision"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }
  }

}
