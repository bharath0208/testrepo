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

import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.model_definitions.ModelDefinitionResource
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineResource
import com.ibm.ml.repository.v4.tests.Sanity
import com.ibm.ml.repository.v4.tests.tags.{Pipelines, SpaceAndProject}
import org.scalatest.DoNotDiscover
import spray.json.{DefaultJsonProtocol, JsObject, _}
import ResourceLoader._

import scala.language.postfixOps

@DoNotDiscover
@Pipelines
@SpaceAndProject
class PipelinesSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {

  //default constructor required for NGPTestReporter
  def this() = this("project", "user", ("", ""))

  private var pipeline: PipelineResource = _

  override def containerType: String = cType

  override def authTokenType: String = tokenType

  override def spaceId = if (cType == "space") Some(containerAndCreator._1) else None

  override def projectId = if (cType == "project") Some(containerAndCreator._1) else None

  override def migrationUserId = containerAndCreator._2

  override def deleteContainer: Boolean = tidyUp

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    pipeline = createPipeline()._1.convertTo[PipelineResource]
    // give DB some time to sync up
    sleep(3000)
  }

  "checking Pipelines API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::PIP-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val swSpecId = createSoftwareSpec()._2
        val hwSpecId = getHardwareSpec()
        val modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id
        val res = createPipeline(fileName="create_request_with_hw_sw_spec.json",
          softwareSpecId=swSpecId,
          hardwareSpecId=Some(hwSpecId),
          modelDefId=Some(modelDefId))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::PIP-CREATE-2:return 400-invalid_request_entity for invalid name" in {
        val res = createPipeline(name = Some(""), expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-CREATE-3:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createPipeline(
          spaceId = Some(""),
          projectId=None,
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-CREATE-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createPipeline(
          spaceId = Some("invalidId"),
          projectId=None,
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::PIP-GET-1:return 200".taggedAs(Sanity) in {
        val res = client.pipelines.getById(id = pipeline.metadata.id, spaceId = spaceId, projectId=projectId, requestId = Some("get-model-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::PIP-GET-2:return 400-invalid_parameter for invalid pipeline id" in {
        val res = client.pipelines.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId=projectId,
          requestId = Some("get-pipeline"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.pipelines.getByIdAsJson(id = pipeline.metadata.id,
          spaceId = Some("invalidId"),
          projectId=None,
          requestId = Some("get-pipeline"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::PIP-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.pipelines.get(spaceId = spaceId, projectId=projectId, requestId = Some("getAll-pipelines"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::PIP-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.pipelines.getAsJson(
          spaceId = Some("invalidId"),
          projectId=None,
          requestId = Some("getAll-pipelines"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.pipelines.getAsJson(
          spaceId = Some(""),
          projectId=None,
          requestId = Some("getAll-pipelines"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::PIP-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.pipelines.update(
          id = pipeline.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId=projectId,
          requestId = Some("update-pipeline"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.get == "newName")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::PIP-UPDATE-2:return 400-invalid_parameter for invalid pipeline id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.pipelines.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId=projectId,
          requestId = Some("update-pipeline"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.pipelines.updateRaw(
          id = pipeline.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          projectId=None,
          requestId = Some("update-pipeline"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.pipelines.updateRaw(
          id = pipeline.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId=projectId,
          requestId = Some("update-pipeline"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }
    }

    "create revision" should {
      s"$containerType|$authTokenType token::PIP-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.pipelines.createRevision(
          id = pipeline.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-pipeline-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::PIP-CREATE-REV-2:return 400-invalid_parameter for invalid pipeline id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.pipelines.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-pipeline-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::PIP-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.pipelines.createRevisionRaw(
          id = pipeline.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-pipeline-revision"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::PIP-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.pipelines.getRevisions(
          id = Some(pipeline.metadata.id),
          spaceId = spaceId,
          projectId=projectId,
          requestId = Some("getAll-pipeline-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::PIP-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.pipelines.createRevision(
            id = pipeline.metadata.id,
            payload = createRevisionPayload,
            requestId = Some("create-pipeline-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.pipelines.getRevisions(
            id = Some(pipeline.metadata.id),
            spaceId = spaceId,
            projectId=projectId,
            limit = Some(i-1),
            requestId = Some("getAll-pipeline-revisions"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }
  }
}
