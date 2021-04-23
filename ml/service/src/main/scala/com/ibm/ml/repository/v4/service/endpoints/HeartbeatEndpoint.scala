/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.endpoints

import com.ibm.analytics.wml.api.v4.common.Metadata
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.repository.RepositoryClient
import com.ibm.analytics.wml.service.utils.authorization.AuthorizationClient
import com.ibm.analytics.wml.utils.clients.http.AkkaHttpClient
import com.ibm.analytics.wml.utils.errors.MLFailures
import com.ibm.analytics.wml.utils.reflect.ClassUtils
import com.ibm.analytics.wml.utils.security.Tenant
import com.ibm.analytics.wml.utils.{Dependency, HeartbeatResponse, ServiceNames}
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils._

import scala.util.{Success, Try}

object HeartbeatEndpoint {
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
      getDependency(name = "ml-utils-service", version = ClassUtils.attributeFromMetaOf[Tenant](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-http-client", version = ClassUtils.attributeFromMetaOf[AkkaHttpClient](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-environments-client", version = ClassUtils.attributeFromMetaOf[EnvironmentsClient](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-repo-client", version = ClassUtils.attributeFromMetaOf[RepositoryClient](ClassUtils.IMPLEMENTATION_VERSION)),
      getDependency(name = "ml-cams-client", version = ClassUtils.attributeFromMetaOf[CAMSClient](ClassUtils.IMPLEMENTATION_VERSION))
    ).flatten
  }

  //private lazy val endpoints: List[Endpoint] = {
  //  ResourceRegistry.getPublicPaths.map(path => Endpoint(uri = path, public = true)).toList
  //}

  private def getServiceUrl(sc: ServiceContext,
                            serviceName: String): Option[String] = {
    serviceName match {
      case ServiceNames.iam =>
        sc.authContext.iamConfig match {
          case Some(iam) =>
            Some(getHost(iam.host, iam.port))
          case None =>
            None
        }
      case ServiceNames.cams =>
        Some(s"${getHost(sc.camsClient.camsHost, sc.camsClient.camsPort)}${com.ibm.analytics.wml.cams.Endpoints.ASSET}")
      case ServiceNames.environments =>
        Some(s"${getHost(sc.environmentsClient.envsHost, sc.environmentsClient.envsPort)}${com.ibm.analytics.wml.environments.Endpoints.SOFTWARE_SPECS}")
      case ServiceNames.spaces =>
        Some(s"${getHost(utils.platformHost.get, utils.platformPort.get)}${AuthorizationClient.Endpoints.SPACES}")
      case ServiceNames.projects =>
        Some(s"${getHost(utils.platformHost.get, utils.platformPort.get)}${AuthorizationClient.Endpoints.PROJECTS}")
      case _ =>
        None
    }
  }

  def getHealthProbeApi(sc: ServiceContext)
                       (specificationVersion: String,
                        implementationVersion: String): Try[HeartbeatResponse] = {
    Success(
      HeartbeatResponse(
        implementationVersion,
        Some(specificationVersion),
        Some(
          List(
            Dependency(serviceName = ServiceNames.iam, serviceUrl = getServiceUrl(sc, ServiceNames.iam)),
            Dependency(serviceName = ServiceNames.cams, serviceUrl = getServiceUrl(sc, ServiceNames.cams)),
            Dependency(serviceName = ServiceNames.environments, serviceUrl = getServiceUrl(sc, ServiceNames.environments)),
            Dependency(serviceName = ServiceNames.spaces, serviceUrl = getServiceUrl(sc, ServiceNames.spaces)),
            Dependency(serviceName = ServiceNames.projects, serviceUrl = getServiceUrl(sc, ServiceNames.projects))
            // used when looking up the instance from the token/instance_id
            // Dependency(ServiceNames.wmlInstances)
            //Dependency(ServiceNames.wmlEvent)
          ) ++ internalDependencies
        ),
        None, // Some(endpoints),
        Some(ServiceNames.wmlRepositoryV4)
      )
    )
  }
}
