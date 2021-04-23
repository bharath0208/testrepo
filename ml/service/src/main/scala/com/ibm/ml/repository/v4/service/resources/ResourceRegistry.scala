/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Route
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.resources.impl._

/**
 * The registry that returns the registered
 * resources that should be instantiated by this server.
 */
object ResourceRegistry {
  // do this for now
  def getPublicPaths: Seq[String] = Seq(
    "/ml/v4/experiments",
    "/ml/v4/functions",
    "/ml/v4/pipelines",
    "/ml/v4/models",
    "/ml/v4/model_definitions",
    "/ml/v4/remote_training_systems",
    "/ml/v4/training_definitions",
    "/ml/v4/deployment_job_definitions"
  )

  /**
   * Get the routes that have been registered.
   */
  def getRoutes(sc: ServiceContext)(implicit system: ActorSystem): Seq[Route] =
    getEndpoints(sc).map(e => e.endpoints)

  /**
   * Get the endpoints that have been registered.
   */
  def getEndpoints(sc: ServiceContext)(implicit system: ActorSystem): Seq[ResourceEndpoint] = Seq(
    ExperimentsResource(sc),
    PipelinesResource(sc),
    ModelsResource(sc),
    FunctionsResource(sc),
    ModelDefinitionsResource(sc),
    RemoteTrainingSystemsResource(sc),
    TrainingDefinitionsResource(sc),
    DeploymentJobDefinitionsResource(sc)
  )
}
