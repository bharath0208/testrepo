/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.service

import akka.actor.ActorSystem
import com.ibm.analytics.wml.service.utils.security.AuthContext
import com.ibm.analytics.wml.utils.ServiceNames
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.ml.repository.v4.migration.cloudant.{MigrationDBMethods, SimpleCloudantClient}
import com.ibm.ml.repository.v4.migration.job.JobsManager
import com.ibm.ml.repository.v4.migration.k8s.MigrationKubeClient
import com.ibm.ml.repository.v4.migration.utils.MigrationConstant
import com.ibm.ml.repository.v4.utils.logging.AccessLogger
import com.ibm.ml.repository.v4.utils.{ContextUtils, _}
import com.typesafe.scalalogging.StrictLogging

import scala.util.Try

trait MigrationServiceContext extends ContextUtils with MigrationConstant with StrictLogging {
  val specificationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationServer]("Specification-Version")
  val implementationVersion: Try[String] = ClassUtils.attributeFromMetaOf[MLRepositoryMigrationServer]("Implementation-Version")

  val authContext: AuthContext = getAuthContext

  // this handles incoming calls as well as out-going auth calls that are made when accepting requests
  val endpointsActorSystem: ActorSystem = getEndpointsActorSystem
  // this handle outgoing calls
  val downstreamActorSystem: ActorSystem = getDownstreamActorSystem

  // the cached HTTP client loader for the authentication
  val authHttp: HttpClientBuilder = getHttpClientBuilder(
    ServiceNames.wmlRepositoryMigrationV4,
    AccessLogger.logAuthentication,
    endpointsActorSystem
  )

  val (dbUrl, dbUsername, dbPassword, dbName) = SimpleCloudantClient.getDBConfig(config)

  val cloudantClient: SimpleCloudantClient = SimpleCloudantClient(dbUrl, dbName, dbUsername, dbPassword, Some(CLOUDANT_DESIGN_DOC_PATH))(downstreamActorSystem)
  val jobsManager: JobsManager = JobsManager(this)(downstreamActorSystem)
  val dbMethods: MigrationDBMethods = MigrationDBMethods(cloudantClient)
  val kubeClient: MigrationKubeClient = MigrationKubeClient()(downstreamActorSystem, this)
}
