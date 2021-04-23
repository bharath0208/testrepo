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

import com.ibm.analytics.wml.api.v4ga.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.functions.FunctionResource
import com.ibm.ml.repository.v4.tests.tags.{Functions, SpaceAndProject}
import com.ibm.ml.repository.v4.tests.Sanity
import org.scalatest.DoNotDiscover
import spray.json._
import ResourceLoader._

import scala.language.postfixOps

@DoNotDiscover
@Functions
@SpaceAndProject
class FunctionsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {
  private var function: FunctionResource = _
  private var baseSoftwareSpecificationId: Option[String] = _
  private var softwareSpecificationId: Option[String] = _

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
    val swSpecs = createSoftwareSpec()
    baseSoftwareSpecificationId = swSpecs._1
    softwareSpecificationId = swSpecs._2
    function = createFunction(swSpecId = softwareSpecificationId)._1.convertTo[FunctionResource]
    // give DB some time to sync up
    sleep(3000)
  }

  "checking Functions API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::FUN-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val res = createFunction(swSpecId = softwareSpecificationId)
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::FUN-CREATE-2:return 201 success for base software_spec" in {
        val res = createFunction(fileName = "create_request_with_base_software_spec.json",swSpecId = baseSoftwareSpecificationId)
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::FUN-CREATE-3:return 404-invalid_request_entity for invalid software_spec id" in {
        val res = createFunction(swSpecId = Some("InvalidId"),
          expectedStatus = Some(404))
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-CREATE-4:return 404-invalid_request_entity for invalid software_spec base_id" in {
        val res = createFunction(fileName = "create_request_with_base_software_spec.json",
          swSpecId = Some("InvalidId"),
          expectedStatus = Some(404))
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-CREATE-5:return 400-invalid_request_entity for invalid name" in {
        val res = createFunction(name = Some(""), expectedStatus = Some(400), swSpecId = Some(""))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-CREATE-6:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createFunction(
          name = Some(""),
          spaceId = Some(""),
          expectedStatus = Some(emptySpaceIdError),
          swSpecId = Some("")
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-CREATE-7:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createFunction(
          name = Some(""),
          spaceId = Some("invalidId"),
          expectedStatus = Some(invalidSpaceIdError),
          swSpecId = Some("")
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::FUN-GET-1:return 200".taggedAs(Sanity) in {
        val res = client.functions.getById(id = function.metadata.id, spaceId = spaceId, projectId = projectId, requestId = Some("get-model-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::FUN-GET-2:return 400-invalid_parameter for invalid function id" in {
        val res = client.functions.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-function"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.functions.getByIdAsJson(id = function.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-function"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::FUN-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.functions.get(spaceId = spaceId, projectId = projectId, requestId = Some("getAll-functions"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::FUN-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.functions.getAsJson(
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("getAll-functions"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.functions.getAsJson(
          spaceId = Some(""),
          projectId = None,
          requestId = Some("getAll-functions"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::FUN-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.functions.update(
          id = function.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model-definition"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.get == "newName")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::FUN-UPDATE-2:return 400-invalid_parameter for invalid function id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.functions.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-model-definition"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.functions.updateRaw(
          id = function.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("update-model-definition"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.functions.updateRaw(
          id = function.metadata.id,
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
      s"$containerType|$authTokenType token::FUN-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.functions.createRevision(
          id = function.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::FUN-CREATE-REV-2:return 400-invalid_parameter for invalid function id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.functions.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::FUN-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.functions.createRevisionRaw(
          id = function.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-model-definition-revision"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::FUN-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.functions.getRevisions(
          id = Some(function.metadata.id),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("getAll-model-definition-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::FUN-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the function
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.functions.createRevision(
            id = function.metadata.id,
            payload = createRevisionPayload,
            requestId = Some("create-function-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.functions.getRevisions(
            id = Some(function.metadata.id),
            spaceId = spaceId,
            projectId = projectId,
            limit = Some(i-1),
            requestId = Some("getAll-function-revision"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }

    "upload content" should {
      s"$containerType|$authTokenType token::FUN-UPLOAD-1:return 204".taggedAs(Sanity) in {
        val createContentPayload = s"sample content"
        val res = client.functions.upload(
          id = function.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-function-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::FUN-UPLOAD-2:return 400-missing_one_of_query_parameter for missing space_id query param" in {
        val createContentPayload = s"sample content"
        val res = client.functions.upload(
          id = function.metadata.id,
          spaceId = None,
          projectId = None,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-function-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::FUN-UPLOAD-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space_id query param" in {
        val createContentPayload = s"sample content"
        val res = client.functions.upload(
          id = function.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          contents = createContentPayload.getBytes(),
          requestId = Some("create-model-definition-content"),
          contentMIMEType = Some("application/zip"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }
//      s"$containerType|$authTokenType token::FUN-UPLOAD-4:return 400-content_type_not_allowed for invalid Content-Type header".taggedAs(Sanity) in {
//        val createContentPayload = s"sample content"
//        val res = client.functions.upload(
//          id = function.metadata.id,
//          spaceId = spaceId,
//          projectId = projectId,
//          contents = createContentPayload.getBytes(),
//          requestId = Some("create-function-content"),
//          contentMIMEType = Some("text/plain"),
//          expectedStatus = Some(400))
//        assert(res._3 == 400)
//        assert("content_type_not_allowed" == getErrorCodeFromResponse(res._1.parseJson))
//      }
    }

    "download content" should {
      s"$containerType|$authTokenType token::FUN-DOWNLOAD-1:return 200".taggedAs(Sanity) in {
        val res = client.functions.download(
          id = function.metadata.id,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-function-content"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::FUN-DOWNLOAD-2:return 400-invalid_parameter for invalid function id" in {
        val res = client.functions.download(
          id = "InvalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-function-content"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::FUN-DOWNLOAD-3:return 400-missing_one_of_query_parameter for missing space id" in {
        val res = client.functions.download(
          id = function.metadata.id,
          spaceId = None,
          projectId = None,
          requestId = Some("get-model-definition-content"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("missing_one_of_query_parameter" == getErrorCodeFromResponse(res._1.parseJson))
      }

      s"$containerType|$authTokenType token::FUN-DOWNLOAD-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid space id" in {
        val res = client.functions.download(
          id = function.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-model-definition-content"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1.parseJson))
      }
    }
  }
}
