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

import com.ibm.analytics.wml.api.v4ga.remote_training_systems.RemoteTrainingSystemJsonFormat._
import com.ibm.analytics.wml.api.v4ga.remote_training_systems.RemoteTrainingSystemResource
import com.ibm.ml.repository.v4.tests.Sanity
import com.ibm.ml.repository.v4.tests.tags.{RemoteTrainingSystems, SpaceAndProject}
import org.scalatest.DoNotDiscover
import spray.json.{DefaultJsonProtocol, JsObject, _}
import ResourceLoader._

import scala.language.postfixOps

@DoNotDiscover
@RemoteTrainingSystems
@SpaceAndProject
class RemoteTrainingSystemsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {

  //default constructor required for NGPTestReporter
  def this() = this("project", "user", ("", ""))

  private var remoteTrainingSystem: RemoteTrainingSystemResource = _

  override def containerType: String = cType

  override def authTokenType: String = tokenType

  override def spaceId = if (cType == "space") Some(containerAndCreator._1) else None

  override def projectId = if (cType == "project") Some(containerAndCreator._1) else None

  override def migrationUserId = containerAndCreator._2

  override def deleteContainer: Boolean = tidyUp

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    remoteTrainingSystem = createRemoteTrainingSystem()._1.convertTo[RemoteTrainingSystemResource]
    // give DB some time to sync up
    sleep(3000)
  }

  "checking RemoteTrainingSystems API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::RTS-CREATE-1:return 201 success".taggedAs(Sanity) in {
        val res = createRemoteTrainingSystem()
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::RTS-CREATE-2:return 400-invalid_request_entity for invalid name" in {
        val res = createRemoteTrainingSystem(name = Some(""), expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-CREATE-3:return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createRemoteTrainingSystem(
          spaceId = Some(""),
          projectId = None,
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-CREATE-4:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createRemoteTrainingSystem(
          spaceId = Some("invalidId"),
          projectId = None,
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::RTS-GET-1:return 200".taggedAs(Sanity) in {
        val res = client.remoteTrainingSystems.getById(id = remoteTrainingSystem.metadata.id, spaceId = spaceId, projectId = projectId, requestId = Some("get-rts"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::RTS-GET-2:return 400-invalid_parameter for invalid RemoteTrainingSystem id" in {
        val res = client.remoteTrainingSystems.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("get-rts"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-GET-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.remoteTrainingSystems.getByIdAsJson(id = remoteTrainingSystem.metadata.id,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("get-rts"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::RTS-GETALL-1:return 200".taggedAs(Sanity) in {
        val res = client.remoteTrainingSystems.get(spaceId = spaceId, projectId = projectId, requestId = Some("getAll-rts"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::RTS-GETALL-2:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.remoteTrainingSystems.getAsJson(
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("getAll-rts"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-GETALL-3:return $emptySpaceIdError-$emptySpaceIdQueryCode for empty spaceId" in {
        val res = client.remoteTrainingSystems.getAsJson(
          spaceId = Some(""),
          projectId = None,
          requestId = Some("getAll-rts"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQueryCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::RTS-UPDATE-1:return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.remoteTrainingSystems.update(
          id = remoteTrainingSystem.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-rts"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.get == "newName")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::RTS-UPDATE-2:return 400-invalid_parameter for invalid remoteTrainingSystem id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.remoteTrainingSystems.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-rts"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-UPDATE-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.remoteTrainingSystems.updateRaw(
          id = remoteTrainingSystem.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          projectId = None,
          requestId = Some("update-rts"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-UPDATE-4:return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.remoteTrainingSystems.updateRaw(
          id = remoteTrainingSystem.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("update-rts"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }
    }

    "create revision" should {
      s"$containerType|$authTokenType token::RTS-CREATE-REV-1:return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.remoteTrainingSystems.createRevision(
          id = remoteTrainingSystem.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-rts-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::RTS-CREATE-REV-2:return 400-invalid_parameter for invalid remoteTrainingSystem id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.remoteTrainingSystems.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-rts-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::RTS-CREATE-REV-3:return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.remoteTrainingSystems.createRevisionRaw(
          id = remoteTrainingSystem.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-rts-revision"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::RTS-GET-REV-1:return 200".taggedAs(Sanity) in {
        val res = client.remoteTrainingSystems.getRevisions(
          id = Some(remoteTrainingSystem.metadata.id),
          spaceId = spaceId,
          projectId = projectId,
          requestId = Some("getAll-rts-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::RTS-GET-REV-2:return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.remoteTrainingSystems.createRevision(
            id = remoteTrainingSystem.metadata.id,
            payload = createRevisionPayload,
            requestId = Some("create-rts-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.remoteTrainingSystems.getRevisions(
            id = Some(remoteTrainingSystem.metadata.id),
            spaceId = spaceId,
            projectId = projectId,
            limit = Some(i-1),
            requestId = Some("getAll-rts-revision"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }
  }
}
