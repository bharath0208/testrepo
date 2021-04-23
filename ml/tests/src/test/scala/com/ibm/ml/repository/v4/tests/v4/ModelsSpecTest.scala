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
import com.ibm.analytics.wml.api.v4ga.models.ModelJsonFormat._
import com.ibm.analytics.wml.api.v4ga.models.{ModelResource, ModelResources}
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineJsonFormat._
import com.ibm.analytics.wml.api.v4ga.pipelines.PipelineResource
import com.ibm.analytics.wml.utils.errors.MLFailures
import com.ibm.ml.repository.v4.tests.tags.{Models, SpaceAndProject}
import com.ibm.ml.repository.v4.tests.Sanity
import org.apache.http.Header
import org.scalatest.DoNotDiscover
import spray.json._
import ResourceLoader._

import scala.util.{Failure, Success, Try}

@DoNotDiscover
@Models
@SpaceAndProject
class ModelsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {
  private var model: ModelResource = _
  private var modelDefId: String = _
  private var softwareSpecificationId: Option[String] = _
  private var modelAttachmentId: String = _

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
    modelDefId = createModelDefinition()._1.convertTo[ModelDefinitionResource].metadata.id

    softwareSpecificationId = createSoftwareSpec()._2

    model = createModel(softwareSpecId = softwareSpecificationId, modelDefId = modelDefId)._1.convertTo[ModelResource]

    Try(createModelAttachment) match {
      case Success(_) => //ok
      case Failure(exception) => fail(s"Failed to create model attachment for tests; ${exception.getMessage}")
    }

    Try {
      val attachmentList = client.models.download(
        id = model.metadata.id,
        spaceId = spaceId,
        projectId = projectId,
        requestId = Some("get-model-attachments-list"),
        expectedStatus = Some(200))._1.parseJson

      import DefaultJsonProtocol._
      val attachments = attachmentList.asJsObject.fields("attachments")
      modelAttachmentId = attachments.convertTo[JsArray].elements.head.asJsObject.fields("attachment_id").convertTo[String]
      logger.info(s"attachment id is $modelAttachmentId. Model id is ${model.metadata.id}. space id is $spaceId")
    } match {
      case Success(_) => //ok
      case Failure(exception) => fail(s"Failed to get model attachment id for tests; ${exception.getMessage}")
    }

    // give DB some time to sync up
    sleep(3000)
  }

  private def createModelAttachment: (String, Array[Header], Int) = {
    client.models.upload(
      id = model.metadata.id,
      spaceId = spaceId,
      projectId = projectId,
      contentFormat = Some("native"),
      contents = s"""{"a":"b"}""".getBytes(),
      requestId = Some("create-model-content"),
      contentMIMEType = Some("application/zip"),
      expectedStatus = Some(201))
  }

  private def checkSchemasNonEmpty(modelResource: ModelResource): Boolean = {
    modelResource.entity.schemas match {
      case Some(schemas) if schemas.input.isEmpty && schemas.output.isEmpty =>
        false
      case Some(_) =>
        true
      case None =>
        true
    }
  }

  private def checkSchemasNonEmpty(modelResources: ModelResources): Boolean = {
    for (mr <- modelResources.resources) {
      if (!checkSchemasNonEmpty(mr))
        return false
    }
    true
  }

  "checking Models API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::MOD-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val res = createModel(softwareSpecId = softwareSpecificationId, modelDefId = modelDefId)
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MOD-CREATE-2:return 201 success for request with pipeline" in {
        val pipelineId = createPipeline()._1.convertTo[PipelineResource].metadata.id
        val res = createModel(fileName="create_request_with_pipeline.json",
          softwareSpecId = softwareSpecificationId,
          modelDefId = modelDefId,
          pipelineId = Some(pipelineId))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MOD-CREATE-3:return 400-invalid_request_entity for invalid name" in {
        val res = createModel(softwareSpecId = softwareSpecificationId, modelDefId = modelDefId, name = "", expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-CREATE-4:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createModel(
          softwareSpecId = softwareSpecificationId,
          modelDefId = modelDefId,
          spaceId = Some(""),
          projectId = None,
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-CREATE-5:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createModel(
          softwareSpecId = softwareSpecificationId,
          modelDefId = modelDefId,
          spaceId = Some("invalidId"),
          projectId = None,
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }

      s"$containerType|$authTokenType token::MOD-CREATE-6:return $invalidJSONError-$invalidJSONCode for invalid model json" in {
        var json: String =
          """
            |{
            |  "name": "my_asset_name",
            |  ">>>CONTAINER_TYPE<<<": ">>>CONTAINER_ID<<<",
            |  "model_definition":{
            |  },
            |  "software_spec" : {
            |  },
            |  "type": "tensorflow_1.15",
            |  "schemas": {
            |    "input": [
            |      {
            |        "fields": [
            |          {
            |            "name": "duration",
            |            "type": "number"
            |          }
            |        ],
            |        "id": "t1",
            |        "name": "Tasks"
            |      }
            |    ],
            |    "output": [
            |      {
            |        "fields": [
            |          {
            |            "name": "duration",
            |            "type": "number"
            |          }
            |        ],
            |        "id": "t1",
            |        "name": "Tasks"
            |      }
            |    ]
            |  }
            |}
            |""".stripMargin

        spaceId.foreach(s => {
          json = json.replaceAll(">>>CONTAINER_TYPE<<<", "space_id")
          json = json.replaceAll(">>>CONTAINER_ID<<<", s)
        })
        projectId.foreach(s => {
          json = json.replaceAll(">>>CONTAINER_TYPE<<<", "project_id")
          json = json.replaceAll(">>>CONTAINER_ID<<<", s)
        })
        json = json.replaceAll(">>>MODEL_DEF_ID<<<", modelDefId)

        val res = client.models.createJson(entity = Some(JsonParser(json)), spaceId=softwareSpecificationId, requestId=Some("create-model-with-bad-json"), expectedStatus=Some(invalidJSONError))
        info(s"Malformed json response: ${logPrint(res._1)}")
        assert(res._3 == invalidJSONError)
        assert(invalidJSONCode == getErrorCodeFromResponse(res._1))
        val failures = res._1.convertTo[MLFailures]
        assert(failures.errors.head.target.isDefined, s"Failed to find target in ${logPrint(failures.toJson)}")
      }

      s"$containerType|$authTokenType token::MOD-CREATE-7:return 201 success for request with empty schemas" in {
        val res = createModel(fileName="create_request_with_empty_schemas.json",
          softwareSpecId = softwareSpecificationId,
          modelDefId = modelDefId)
        assert(res._3 == 201)
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::MOD-GET-1:return 200".taggedAs(Sanity) in {
        val res = client.models.getByIdAsJson(id = model.metadata.id, spaceId = spaceId, projectId = projectId, requestId = Some("get-model"))
        assert(res._3 == 200)
        assert(checkSchemasNonEmpty(res._1.convertTo[ModelResource]), res._1.prettyPrint)
      }

      s"$containerType|$authTokenType token::MOD-GET-2:return 400-invalid_parameter for invalid model definition id" in {
        val res = client.models.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-model"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.models.getByIdAsJson(id = model.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-model"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::MOD-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.models.getAsJson(spaceId = spaceId, projectId = projectId, requestId = Some("getAll-model-definition"))
        assert(res._3 == 200)
        assert(checkSchemasNonEmpty(res._1.convertTo[ModelResources]), res._1.prettyPrint)
      }

      s"$containerType|$authTokenType token::MOD-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.models.getAsJson(
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("getAll-model-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.models.getAsJson(
          spaceId = Some(""),
          projectId = None,
          requestId = Some("getAll-model-definition"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::MOD-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.models.updateRaw(
          id = model.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model"))
        assert(res._3 == 200)
        val modelRes = res._1.convertTo[ModelResource]
        import DefaultJsonProtocol._
        assert(modelRes.metadata.name.get == "newName")
        assert(modelRes.entity.custom.map(_.fields.get("testKey")).get.get.convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::MOD-UPDATE-2:return 400-invalid_parameter for invalid model id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.models.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.models.updateRaw(
          id = model.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("update-model"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.models.updateRaw(
          id = model.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }
    }

    "create revision" should {
      s"$containerType|$authTokenType token::MOD-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.models.createRevisionRaw(
          id = model.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          payload = createRevisionPayload,
          requestId = Some("create-model-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MOD-CREATE-REV-2:return 400-invalid_parameter for invalid model id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.models.createRevisionRaw(
          id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::MOD-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("InvalidId"),
          projectId = None
        )
        val res = client.models.createRevisionRaw(
          id = model.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          payload = createRevisionPayload,
          requestId = Some("create-model-revision"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::MOD-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.models.getRevisions(
          id = Some(model.metadata.id),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("getAll-model-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MOD-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.models.createRevisionRaw(
            id = model.metadata.id,
            spaceId = spaceId,
            projectId = projectId,
            payload = createRevisionPayload,
            requestId = Some("create-model-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.models.getRevisions(
            id = Some(model.metadata.id),
            spaceId = spaceId,
            projectId = projectId,
            limit = Some(i-1),
            requestId = Some("getAll-model-revision"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }

    "upload content" should {
      s"$containerType|$authTokenType token::MOD-UPLOAD-1:return 204".taggedAs(Sanity) in {
        val createContentPayload = s"sample content"
        val res = client.models.upload(
          id = model.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(201))
        import spray.json.DefaultJsonProtocol._
        modelAttachmentId = res._1.parseJson.asJsObject.fields("attachment_id").convertTo[String]
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::MOD-UPLOAD-2:return 400-missing_query_parameter for missing content_type query param" in {
        val createContentPayload = s"sample content"
        val res = client.models.upload(
          id = model.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          contents = createContentPayload.getBytes(),
          contentFormat = None,
          requestId = Some("create-model-content"),
          contentMIMEType = Some("application/xml"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MOD-UPLOAD-3:return 400-missing_one_of_query_parameter for missing space_id query param" in {
        val createContentPayload = s"sample content"
        val res = client.models.upload(
          id = model.metadata.id,
          spaceId = None,
          projectId = None,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          contentMIMEType = Some("application/xml"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MOD-UPLOAD-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space_id query param" in {
        val createContentPayload = s"sample content"
        val res = client.models.upload(
          id = model.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          contentMIMEType = Some("application/xml"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }


//      s"$containerType|$authTokenType token::MOD-UPLOAD-5:return 400-content_type_not_allowed for invalid Content-Type header" in {
//        val createContentPayload = s"sample content"
//        val res = client.models.upload(
//          id = model.metadata.id,
//          spaceId = spaceId,
//          projectId = projectId,
//          contents = createContentPayload.getBytes(),
//          requestId = Some("create-model-definition-content"),
//          contentMIMEType = Some("application/csv"),
//          expectedStatus = Some(400))
//        assert(res._3 == 400)
//        assert("content_type_not_allowed_for_model_type" == getErrorCodeFromResponse(res._1.parseJson))
//      }

      /*s"$containerType|$authTokenType token::MOD-UPLOAD-6:return 400-content_type_missing for missing Content-Type header" in {
        val createContentPayload = s"sample content"
        val res = client.models.upload(
          id = model.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("content_type_missing" == getErrorCodeFromResponse(res._1.parseJson))
      }*/
    }

    "get content attachments" should {
      s"$containerType|$authTokenType token::MOD-DOWNLOAD-1:return 200".taggedAs(Sanity) in {
        val res = client.models.download(
          id = model.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-model-attachments-list"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-2:return 400-invalid_parameter for invalid model id" in {
        val res = client.models.download(
          id = "InvalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-model-attachments-list"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-3:return 400-missing_one_of_query_parameter for missing space id" in {
        val res = client.models.download(
          id = model.metadata.id,
          spaceId = None,
          projectId = None,
          requestId = Some("get-model-attachments-list"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space id" in {
        val res = client.models.download(
          id = model.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-model-attachments-list"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }
    }

    "download content attachments" should {
      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-1:return 200".taggedAs(Sanity) in {
        val res1 = client.models.downloadAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("download-model-attachment"))
        assert(res1._3 == 200)
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-2:return 400-invalid_parameter for invalid model id" in {
        val res = client.models.downloadAttachment(
          id = "InvalidId",
          attachmentId = modelAttachmentId,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("download-model-attachment"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-3:return 400-invalid_parameter for invalid attachment id" in {
        val res = client.models.downloadAttachment(
          id = model.metadata.id,
          attachmentId = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("download-model-attachment"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      "MOD-DOWNLOAD-ATT-4:return 400-missing_one_of_query_parameter for missing space id" in {
        val res = client.models.downloadAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = None,
          projectId = None,
          requestId = Some("download-model-attachment"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-5:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space id" in {
        val res = client.models.downloadAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-model-definition-content"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }

      // test downloadSingleAttachment
      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-6-search:return 200".taggedAs(Sanity) in {
        val res = client.models.downloadSingleAttachment(
          id = model.metadata.id,
          contentFormat = Some("native"),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("download-single-model-attachment")
        )
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-7-search-bad:return 404".taggedAs(Sanity) in {
        val res = client.models.downloadSingleAttachment(
          id = model.metadata.id,
          contentFormat = Some("bad-type"),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("download-single-model-attachment"),
          expectedStatus = Some(404)
        )
        assert(res._3 == 404)
      }
    }

    "delete content attachments" should {
      s"$containerType|$authTokenType token::MOD-DELETE-ATT-1:return 204".taggedAs(Sanity) in {
        val res1 = client.models.deleteAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("delete-model-attachment"))
        assert(res1._3 == 204)
        info(s"Deleted model attachment $modelAttachmentId")
      }

      sleepAfterDelete()

      // https://github.ibm.com/NGP-TWC/ml-planning/issues/17354
      s"$containerType|$authTokenType token::MOD-DOWNLOAD-ATT-AFTER_DELETE-1:return 404" in {
        val res1 = client.models.downloadAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("download-model-attachment-after-delete"),
          expectedStatus = Some(404))
        assert(res1._3 == 404)
      }

      s"$containerType|$authTokenType token::MOD-DELETE-ATT-2:return 404-does_not_exist for deleted attachment id" in {
        val res = client.models.deleteAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("delete-model-attachment"),
          expectedStatus = Some(404))

        assert(res._3 == 404)
        assert("does_not_exist" == getErrorCodeFromResponse(res._1.getOrElse(fail("invalid response body")).parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DELETE-ATT-3:return 400-invalid_parameter for invalid model id" in {
        val res = client.models.deleteAttachment(
          id = "InvalidId",
          attachmentId = modelAttachmentId,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("delete-model-attachment"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.getOrElse(fail("invalid response body")).parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DELETE-ATT-4:return 400-invalid_parameter for invalid attachment id" in {
        val res = client.models.deleteAttachment(
          id = model.metadata.id,
          attachmentId = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("delete-model-attachment"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.getOrElse(fail("invalid response body")).parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DELETE-ATT-5:return 400-missing_one_of_query_parameter for missing space id" in {
        val res = client.models.deleteAttachment(
          id = model.metadata.id,
          attachmentId = modelAttachmentId,
          spaceId = None,
          projectId = None,
          requestId = Some("delete-model-attachment"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.getOrElse(fail("invalid response body")).parseJson))
      }

      s"$containerType|$authTokenType token::MOD-DELETE-ATT-6:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space id" in {
        val res = client.models.deleteAttachment(
          id = model.metadata.id,
          attachmentId=modelAttachmentId,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("delete-model-attachment"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.getOrElse(fail("invalid response body")).parseJson))
      }
    }
  }
}
