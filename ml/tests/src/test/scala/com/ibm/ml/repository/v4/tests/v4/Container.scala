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

import java.util.UUID

import com.ibm.analytics.spaces.api.v2.SpacesJsonFormat._
import com.ibm.analytics.spaces.api.v2._
import com.ibm.ml.repository.v4.tests.utils.FormatUtils._
import com.ibm.ml.repository.v4.tests.utils.V4TestServicesClient
import com.ibm.ml.repository.v4.tests.v4.ResourceLoader._
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.language.postfixOps
import scala.util.control.Breaks


object Container extends StrictLogging {
  private var space: Option[(String, String)] = None
  private var project: Option[(String, String)] = None

  def getSpace(authTokenType: String, withCompute: Boolean = true) = {
    if(!space.isDefined) {
      val config = ConfigFactory.load()
      val client = V4TestServicesClient.getV4Client(config = config, containerType = "space", authTokenType = authTokenType)
      val deploymentType = config.getString("client.auth")
      space = Some(Space(client, config, deploymentType, withCompute).create)
      logger.info(s"CREATED SPACE ${space.get._1}")
      space.get
    } else {
      space.get
    }
  }

  def deleteSpace = {
    val config = ConfigFactory.load()
    val client = V4TestServicesClient.getV4Client(config = config, containerType = "space", authTokenType = "service-id")
    space.map(s => client.spacesv2.delete(id = s._1, requestId = Some("delete-space"), expectedStatus = Some(202)))
  }

  def getProject(authTokenType: String) = {
    if(!project.isDefined) {
      val config = ConfigFactory.load()
      val client = V4TestServicesClient.getV4Client(config = config, containerType = "project", authTokenType = authTokenType)
      val deploymentType = config.getString("client.auth")
      val entity = {
        if (deploymentType == "IAM") {
          loadFromFile(fileName = "projects/create_request_iam.json",  resourceCrn = Some(stripQuotes(config.getString("fvt.cos.resource_crn"))))
        }else {
          loadFromFile(fileName = "projects/create_request_icp.json")
        }
      }
      project = Some(Project(client, config, deploymentType).create(entity))
      logger.info(s"CREATED PROJECT ${project.get._1}")
      project.get
    } else {
      project.get
    }
  }

  def deleteProject = {
    val config = ConfigFactory.load()
    val client = V4TestServicesClient.getV4Client(config = config, containerType = "project", authTokenType = "service-id")
    project.map(p => client.projectsTransactional.delete(id = p._1, requestId = Some("delete-project")))
  }

}

case class Project(client: V4TestServicesClient,
                   config: Config,
                   deploymentType: String) extends StrictLogging {

  def create(entity: JsValue): (String, String) = {
    import DefaultJsonProtocol._
    val res = client.projectsTransactional.createJson(entity = Some(entity), requestId = Some(s"create-project-$deploymentType"))
    val projectId = res._1.asJsObject.fields.getOrElse("location", throw new Exception("Failed to create project")).convertTo[String].split("/").last
    val migrationUserId = client.projects.getById(id = projectId)._1
      .asJsObject.fields.getOrElse("entity", throw new Exception(s"GET failed for project $projectId"))
      .asJsObject.fields.getOrElse("creator_iam_id", throw new Exception(s"Could not get creator id for project $projectId"))
      .convertTo[String]

    (projectId, migrationUserId)
  }
}

case class Space(client: V4TestServicesClient,
                 config: Config,
                 deploymentType: String,
                 withCompute: Boolean) extends StrictLogging {

  def create: (String, String) = {
    val name = s"ml-repository-test-space-${UUID.randomUUID().toString}"
    val cosCrn = stripQuotes(config.getString("fvt.cos.resource_crn"))
    val instanceCrn = stripQuotes(config.getString("fvt.wml.instance_crn"))
    if (deploymentType == "IAM") {
      val storage = Some(
        SpaceStorageRequest(
          cosCrn,
          Some(false)
        )
      )
      def compute: Option[Vector[SpaceComputeRequest]] = {
        if (withCompute) {
          Some(
            Vector(
              SpaceComputeRequest(
                name = "wml-instance",
                crn = instanceCrn
              )
            )
          )
        } else
          None
        // TODO
        None
      }
      val res = client.spacesv2.createJson(
        entity = Some(
          SpaceEntityRequest(
            name = name,
            description = None,
            storage = storage,
            compute = compute
          ).toJson
        ),
        requestId = Some("create-space"),
        expectedStatus = Some(202)
      )._1
      val spaceId = res.asJsObject.fields("metadata").asJsObject.fields("id").convertTo[String]
      var resource: Option[SpaceResource] = None
      // not very elegant but it should work
      // we need to wait whilst the space is being prepared
      val loop = new Breaks
      val now = System.currentTimeMillis()
      loop.breakable {
        val timeout = now + (1000 * 60 * 2)
        do {
          if (System.currentTimeMillis() > timeout)
            loop.break()
          Thread.sleep(1000)
          resource = Some(
            client.spacesv2.getById(
              id = spaceId,
              requestId = Some("get-space"),
              expectedStatus = Some(200)
            )._1
          )
        } while (resource.isDefined && resource.get.entity.status.state == PreparingState)
      }
      assert(resource.isDefined)
      val msg = s"Created space $spaceId with state ${resource.get.entity.status.state.name} after ${formatAsDuration(System.currentTimeMillis() - now)} seconds"
      //info(msg)
      logger.info(msg)
      val failures = if (resource.get.entity.status.failure.isDefined) s" and ${resource.get.entity.status.failure.get.toJson.prettyPrint}" else ""
      assert(resource.get.entity.status.state == ActiveState, s"Creating the space failed with state '${resource.get.entity.status.state.name}'$failures")
      val migrationUserId = resource.get.metadata.creatorId
      (spaceId, migrationUserId)
    } else {
      val spaceId = client.spacesv2.create(
        entity = Some(
          SpaceEntityRequest(
            name = name,
            description = None,
            storage = None,
            compute = None)
        ),
        expectedStatus = Seq(202),
        requestId = Some("create-space")
      )._1.metadata.id
      (spaceId, "")
    }
  }
}
