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
import com.ibm.ml.repository.v4.tests.Sanity
import com.ibm.ml.repository.v4.tests.tags.{ModelDefinitions, SpaceAndProject}
import org.scalatest.DoNotDiscover
import spray.json._
import ResourceLoader._

@DoNotDiscover
@ModelDefinitions
@SpaceAndProject
class ModelDefinitionsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {

  private var modelDef: ModelDefinitionResource = _

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
    modelDef = createModelDefinition()._1.convertTo[ModelDefinitionResource]
    // give DB some time to sync up
    sleep(3000)
  }

  "checking Model Definitions API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::MDEF-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val res = createModelDefinition()
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-2:return 400-invalid_request_entity for invalid name" in {
        val res = createModelDefinition(name = "", expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-3:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createModelDefinition(
          spaceId = Some(""),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createModelDefinition(
          spaceId = Some("invalidId"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-5:return 400-invalid_request_entity for invalid package version" in {
        val res = createModelDefinition(version = "", expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-6:return 400-invalid_request_entity for invalid platform name" in {
        val res = createModelDefinition(platformName = "", expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::MDEF-GET-1:return 200".taggedAs(Sanity) in {
        val res = client.modelDefinitions.getById(id = modelDef.metadata.id, spaceId = spaceId, projectId = projectId, requestId = Some("get-model-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MDEF-GET-2:return 400-invalid_parameter for invalid model definition id" in {
        val res = client.modelDefinitions.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-model-definition"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.modelDefinitions.getByIdAsJson(id = modelDef.metadata.id,
          spaceId = Some("invalidId"),
          requestId = Some("get-model-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::MDEF-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.modelDefinitions.get(spaceId = spaceId, projectId = projectId, requestId = Some("getAll-model-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MDEF-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.modelDefinitions.getAsJson(
          spaceId = Some("invalidId"),
          requestId = Some("getAll-model-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.modelDefinitions.getAsJson(
          spaceId = Some(""),
          requestId = Some("getAll-model-definition"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::MDEF-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.modelDefinitions.update(
          id = modelDef.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model-definition"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.get == "newName")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::MDEF-UPDATE-2:return 400-invalid_parameter for invalid mode definition id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.modelDefinitions.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model-definition"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.modelDefinitions.updateRaw(
          id = modelDef.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          requestId = Some("update-model-definition"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.modelDefinitions.updateRaw(
          id = modelDef.metadata.id,
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
      s"$containerType|$authTokenType token::MDEF-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.modelDefinitions.createRevision(
          id = modelDef.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-REV-2:return 400-invalid_parameter for invalid model definition id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.modelDefinitions.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MDEF-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.modelDefinitions.createRevisionRaw(
          id = modelDef.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::MDEF-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.modelDefinitions.getRevisions(
          id = Some(modelDef.metadata.id),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("getAll-model-definition-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MDEF-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        for(i <- 1 to 3) {
          client.modelDefinitions.createRevision(
            id = modelDef.metadata.id,
            spaceId = spaceId,
            projectId = projectId,
            payload = createRevisionPayload,
            requestId = Some("create-model-definition-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.modelDefinitions.getRevisions(
            id = Some(modelDef.metadata.id),
            spaceId = spaceId,
            projectId = projectId,
            limit = Some(i-1),
            requestId = Some("getAll-model-definition-revision"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }

    "upload content" should {
      s"$containerType|$authTokenType token::MDEF-UPLOAD-1:return 204".taggedAs(Sanity) in {
        val createContentPayload = s"sample content"
        val res = client.modelDefinitions.upload(
          id = modelDef.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MDEF-UPLOAD-2:return 400-missing_one_of_query_parameter for missing space_id query param" in {
        val createContentPayload = s"sample content"
        val res = client.modelDefinitions.upload(
          id = modelDef.metadata.id,
          spaceId = None,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MDEF-UPLOAD-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space_id query param" in {
        val createContentPayload = s"sample content"
        val res = client.modelDefinitions.upload(
          id = modelDef.metadata.id,
          spaceId = Some("invalidId"),
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }
//      s"$containerType|$authTokenType token::MDEF-UPLOAD-4:return 400-content_type_not_allowed for invalid Content-Type header" in {
//        val createContentPayload = s"sample content"
//        val res = client.modelDefinitions.upload(
//          id = modelDef.metadata.id,
//          spaceId = spaceId,
//          projectId = projectId,
//          contents = createContentPayload.getBytes(),
//          requestId = Some("create-model-definition-content"),
//          contentMIMEType = Some("text/plain"),
//          expectedStatus = Some(400))
//        assert(res._3 == 400)
//        assert("content_type_not_allowed" == getErrorCodeFromResponse(res._1.parseJson))
//      }
    }

    "download content" should {
      s"$containerType|$authTokenType token::MDEF-DOWNLOAD-1:return 200".taggedAs(Sanity) in {
        val res = client.modelDefinitions.download(
          id = modelDef.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("create-model-definition-content"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MDEF-DOWNLOAD-2:return 400-invalid_parameter for invalid modelDef id" in {
        val res = client.modelDefinitions.download(
          id = "InvalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("create-model-definition-content"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MDEF-DOWNLOAD-3:return 400-missing_one_of_query_parameter for missing space id" in {
        val res = client.modelDefinitions.download(
          id = modelDef.metadata.id,
          spaceId = None,
          requestId = Some("get-model-definition-content"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MDEF-DOWNLOAD-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space id" in {
        val res = client.modelDefinitions.download(
          id = modelDef.metadata.id,
          spaceId = Some("invalidId"),
          requestId = Some("get-model-definition-content"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }
    }
  }
}
