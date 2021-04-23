/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.api.internal

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.{Directives, Route}
import akka.util.ByteString
import com.ibm.analytics.wml.api.v4.common.Metadata
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.kubeclient.KubernetesClient
import com.ibm.analytics.wml.repository.RepositoryClient
import com.ibm.analytics.wml.service.utils.authorization.AuthorizationClient
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.HeartbeatJsonFormat._
import com.ibm.analytics.wml.utils.clients.http.AkkaHttpClient
import com.ibm.analytics.wml.utils.errors.MLFailures
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.analytics.wml.utils.{Dependency, HeartbeatResponse, ServiceNames}
import com.ibm.ml.repository.v4.migration.service.MigrationServiceContext
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils.{APIUtils, getHost}
import spray.json._

import scala.util.{Failure, Success, Try}

/* the endpoints for the migration service health probe */
case class HeartbeatAPI(msc: MigrationServiceContext)
                       (specificationVersion: String,
                        implementationVersion: String) extends Directives with APIUtils {
  // add the main dependencies here so that they are easy to verify
  private lazy val internalDependencies: List[Dependency] = {
    def getDependency(name: String, version: Try[String]): Option[Dependency] = {
      if (version.isSuccess)
        Some(Dependency(serviceName = name, version = Some(version.get)))
      else
        None
    }

    List(
      getDependency(name = "ml-utils-api", version = ClassUtils.attributeFromMetaOf[Metadata](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-utils-common", version = ClassUtils.attributeFromMetaOf[MLFailures](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-utils-service", version = ClassUtils.attributeFromMetaOf[Identity](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-http-client", version = ClassUtils.attributeFromMetaOf[AkkaHttpClient](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-environments-client", version = ClassUtils.attributeFromMetaOf[EnvironmentsClient](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-repo-client", version = ClassUtils.attributeFromMetaOf[RepositoryClient](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-kube-client", version = ClassUtils.attributeFromMetaOf[KubernetesClient](ClassUtils.IMPLEMENTATION_VERSION))
    ).flatten
  }

  private def getServiceUrl(msc: MigrationServiceContext,
                            serviceName: String): Option[String] = {
    serviceName match {
      case ServiceNames.iam =>
        msc.authContext.iamConfig match {
          case Some(iam) =>
            Some(getHost(iam.host, iam.port))
          case None =>
            None
        }
      case ServiceNames.environments =>
        Some(s"${getHost(utils.platformHost.get, utils.platformPort.get)}${com.ibm.analytics.wml.environments.Endpoints.SOFTWARE_SPECS}")
      case ServiceNames.spaces =>
        Some(s"${getHost(utils.platformHost.get, utils.platformPort.get)}${AuthorizationClient.Endpoints.SPACES}")
      case ServiceNames.projects =>
        Some(s"${getHost(utils.platformHost.get, utils.platformPort.get)}${AuthorizationClient.Endpoints.PROJECTS}")
      case _ =>
        None
    }
  }

  private def getHealthProbeApi(msc: MigrationServiceContext)
                               (specificationVersion: String,
                                implementationVersion: String): Try[HeartbeatResponse] = {
    Success(
      HeartbeatResponse(
        implementationVersion,
        Some(specificationVersion),
        Some(
          List(
            Dependency(serviceName = ServiceNames.iam, serviceUrl = getServiceUrl(msc, ServiceNames.iam)),
            Dependency(serviceName = ServiceNames.environments, serviceUrl = getServiceUrl(msc, ServiceNames.environments)),
            Dependency(serviceName = ServiceNames.spaces, serviceUrl = getServiceUrl(msc, ServiceNames.spaces)),
            Dependency(serviceName = ServiceNames.projects, serviceUrl = getServiceUrl(msc, ServiceNames.projects))
            // used when looking up the instance from the token/instance_id
            // Dependency(ServiceNames.wmlInstances)
            //Dependency(ServiceNames.wmlEvent)
          ) ++ internalDependencies
        ),
        None,
        Some(ServiceNames.wmlRepositoryMigrationV4)
      )
    )
  }

  /* The endpoints for the health probe API. */
  val endpoints: Route = {
    respondWithDefaultHeaders(getNoCacheResponseHeaders) {
      getHealthProbe
    }
  }

  /* /ml/wml_services/ml-repository-migration/heartbeat */
  def getHealthProbe: Route = {
    path("ml" / "wml_services" / "ml-repository-migration" / "heartbeat") {
      get {
        val heartbeat = getHealthProbeApi(msc)(specificationVersion, implementationVersion)
        heartbeat match {
          case Success(ok) =>
            complete(
              HttpResponse(
                status = StatusCodes.OK,
                entity = HttpEntity(`application/json`, ByteString(ok.toJson.prettyPrint))
              )
            )
          case Failure(exception) =>
            failWith(exception)
        }
      }
    }
  }
}
