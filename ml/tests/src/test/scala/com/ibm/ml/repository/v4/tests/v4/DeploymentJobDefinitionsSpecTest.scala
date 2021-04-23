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
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat._
import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionResource
import com.ibm.ml.repository.v4.tests.Sanity
import com.ibm.ml.repository.v4.tests.tags.{DeploymentJobDefinitions, SpaceOnly}
import org.scalatest.DoNotDiscover
import spray.json.{DefaultJsonProtocol, JsObject, _}
import ResourceLoader._

import scala.language.postfixOps

@DoNotDiscover
@DeploymentJobDefinitions
@SpaceOnly
class DeploymentJobDefinitionsSpecTest(cType: String, tokenType: String, containerAndCreator: (String, String), tidyUp: Boolean = true) extends MLRepoTestBase {
  private var hardwareSpecId: String = _
  private var dataAssetId: Option[String] = _
  private var deploymentJobDefinition: DeploymentJobDefinitionResource = _

  //default constructor required for NGPTestReporter
  def this() = this("project", "user", ("", ""))

  override def containerType: String = cType

  override def authTokenType: String = tokenType

  override def spaceId = if (cType == "space") Some(containerAndCreator._1) else None

  override def projectId = if (cType == "project") Some(containerAndCreator._1) else None

  override def migrationUserId = containerAndCreator._2

  override def deleteContainer = tidyUp

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    hardwareSpecId = getHardwareSpec()
    dataAssetId = createDataAsset()._1.convertTo[Asset].metadata.assetId
    deploymentJobDefinition = createDeploymentJobDefinition(
      hardwareSpecId = Some(hardwareSpecId),
      dataAssetId = dataAssetId
    )._1.convertTo[DeploymentJobDefinitionResource]
    // give DB some time to sync up
    sleep(3000)
  }

  "checking DeploymentJobDefinitions API for "+containerType when {
    "create" should {
      s"$containerType|$authTokenType token::return 201 success".taggedAs(Sanity) in {
        val res = createDeploymentJobDefinition(
          hardwareSpecId = Some(hardwareSpecId),
          dataAssetId = dataAssetId
        )
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::return 404-invalid_request_entity for invalid hardwareSpec id" in {
        val res = createDeploymentJobDefinition(
          hardwareSpecId = Some("invalidId"),
          dataAssetId = dataAssetId,
          expectedStatus = Some(404)
        )
        assert(res._3 == 404)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return 400-ReservedValue for invalid dataAssetId" in {
        val res = createDeploymentJobDefinition(
          hardwareSpecId = Some(hardwareSpecId),
          dataAssetId = Some("invalidId"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("ReservedValue" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return $emptySpaceIdError-$emptySpaceIdEntityCode for empty spaceId" in {
        val res = createDeploymentJobDefinition(spaceId = Some(""),
          hardwareSpecId = Some(hardwareSpecId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdEntityCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = createDeploymentJobDefinition(spaceId = Some("InvalidId"),
          hardwareSpecId = Some(hardwareSpecId),
          dataAssetId = dataAssetId,
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get" should {
      s"$containerType|$authTokenType token::return 200".taggedAs(Sanity) in {
        val res = client.deploymentJobDefinitions.getById(id = deploymentJobDefinition.metadata.id,
          spaceId = spaceId,
          requestId = Some("get-deployment-job-definition"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::return 400-invalid_parameter for invalid deployment job definition id" in {
        val res = client.deploymentJobDefinitions.getByIdAsJson(id = "invalidId",
          spaceId = spaceId,
          requestId = Some("get-deployment-job-definition"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.deploymentJobDefinitions.getByIdAsJson(id = deploymentJobDefinition.metadata.id,
          spaceId = Some("invalidId"),
          requestId = Some("get-deployment-job-definition"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return 400-asset_type_mismatch for a assetId from other asset" in {
        val res = client.deploymentJobDefinitions.getByIdAsJson(id = dataAssetId.get,
          spaceId = spaceId,
          requestId = Some("get-deployment-job-definition"),
          expectedStatus = Some(400)
        )
        assert(res._3 == 400)
        assert("asset_type_mismatch" == getErrorCodeFromResponse(res._1))
      }
    }

    "getAll" should {
      s"$containerType|$authTokenType token::return 200".taggedAs(Sanity) in {
        val res = client.deploymentJobDefinitions.get(spaceId = spaceId, requestId = Some("getAll-deployment-job-definitions"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val res = client.deploymentJobDefinitions.getAsJson(
          spaceId = Some("invalidId"),
          requestId = Some("getAll-deployment-job-definitions"),
          expectedStatus = Some(invalidSpaceIdError)
        )
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return $emptySpaceIdError-$emptySpaceIdQuerySpaceCode for empty spaceId" in {
        val res = client.deploymentJobDefinitions.getAsJson(
          spaceId = Some(""),
          requestId = Some("getAll-deployment-job-definitions"),
          expectedStatus = Some(emptySpaceIdError)
        )
        assert(res._3 == emptySpaceIdError)
        assert(emptySpaceIdQuerySpaceCode == getErrorCodeFromResponse(res._1))
      }
    }

    "update" should {
      s"$containerType|$authTokenType token::return 200".taggedAs(Sanity) in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue"}},
             |{"op": "replace", "path":"/name", "value": "newName"}
             |]""".stripMargin.parseJson
        val res = client.deploymentJobDefinitions.update(
          id = deploymentJobDefinition.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          requestId = Some("update-deployment-job-definitions"))
        assert(res._3 == 200)
        assert(res._1.metadata.name.isDefined, s"failed to find name in ${logPrint(res._1.metadata.toJson)}")
        assert(res._1.metadata.name.get == "newName")
        assert(res._1.entity.custom.isDefined, s"failed to find custom object in ${logPrint(res._1.entity.toJson)}")
        import DefaultJsonProtocol._
        assert(res._1.entity.custom.get.fields("testKey").convertTo[String] == "testValue")
      }

      s"$containerType|$authTokenType token::return 400-invalid_parameter for invalid deployment-job-definition id" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.deploymentJobDefinitions.updateRaw(
          id = "invalidId",
          patch = patchPayload,
          spaceId = spaceId,
          requestId = Some("update-deployment-job-definition"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val patchPayload =
          s"""[
             |{"op": "add", "path":"/custom", "value": {"testKey":"testValue5"}},
             |{"op": "replace", "path":"/name", "value": "newName5"}
             |]""".stripMargin.parseJson
        val res = client.deploymentJobDefinitions.updateRaw(
          id = deploymentJobDefinition.metadata.id,
          patch = patchPayload,
          spaceId = Some("invalidId"),
          requestId = Some("update-deployment-job-definitions"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(invalidSpaceIdCode == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return 400-invalid_request_entity for empty patch list" in {
        val patchPayload =
          s"""[]""".stripMargin.parseJson
        val res = client.deploymentJobDefinitions.updateRaw(
          id = deploymentJobDefinition.metadata.id,
          patch = patchPayload,
          spaceId = spaceId,
          requestId = Some("update-deployment-job-definitions"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_request_entity" == getErrorCodeFromResponse(res._1))
      }
    }

    "create revision" should {
      s"$containerType|$authTokenType token::return 201".taggedAs(Sanity) in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.deploymentJobDefinitions.createRevision(
          id = deploymentJobDefinition.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-deployment-job-definition-revision"),
          expectedStatus = Some(201))
        assert(res._3 == 201)
      }

      s"$containerType|$authTokenType token::return 400-invalid_parameter for invalid deployment-job-definition id" in {
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)
        val res = client.deploymentJobDefinitions.createRevisionRaw(
          id = "invalidId",
          payload = createRevisionPayload,
          requestId = Some("create-deployment-job-definition-revision"),
          expectedStatus = Some(400))
        assert(res._3 == 400)
        assert("invalid_parameter" == getErrorCodeFromResponse(res._1))
      }

      s"$containerType|$authTokenType token::return $invalidSpaceIdError-$invalidSpaceIdCode for invalid spaceId" in {
        val createRevisionPayload = loadFromFile(
          fileName = "common/create_revision_request.json",
          spaceId = Some("invalidId"),
          projectId = None
        )
        val res = client.deploymentJobDefinitions.createRevisionRaw(
          id = deploymentJobDefinition.metadata.id,
          payload = createRevisionPayload,
          requestId = Some("create-deployment-job-definition-revision"),
          expectedStatus = Some(invalidSpaceIdError))
        assert(res._3 == invalidSpaceIdError)
        assert(getErrorCodeFromResponse(res._1) == invalidSpaceIdInRequestBodyCode)
      }
    }

    "get revisions" should {
      s"$containerType|$authTokenType token::return 200".taggedAs(Sanity) in {
        val res = client.deploymentJobDefinitions.getRevisions(
          id = Some(deploymentJobDefinition.metadata.id),
          spaceId = spaceId,
          requestId = Some("getAll-deployment-job-definition-revision"))
        assert(res._3 == 200)
      }

      s"$containerType|$authTokenType token::return 200 and size of response is equal to the limit in the request" in {
        //create another revision so that there are two revisions for the modelDef
        val createRevisionPayload = loadFromFile(fileName = "common/create_revision_request.json", spaceId = spaceId, projectId = projectId)

        for(i <- 1 to 3) {
          client.deploymentJobDefinitions.createRevision(
            id = deploymentJobDefinition.metadata.id,
            payload = createRevisionPayload,
            requestId = Some("create-deployment-job-definitions-revision"),
            expectedStatus = Some(201))

          // give Cloudant the time to replicate
          sleep(1000)

          val res = client.deploymentJobDefinitions.getRevisions(
            id = Some(deploymentJobDefinition.metadata.id),
            spaceId = spaceId,
            limit = Some(i-1),
            requestId = Some("getAll-deployment-job-definition-revisions"))
          assert(res._3 == 200)
          import DefaultJsonProtocol._
          val response = res._1.asJsObject().fields("resources")
          assert(response.convertTo[List[JsObject]].size == i-1)
        }
      }
    }
  }
}
