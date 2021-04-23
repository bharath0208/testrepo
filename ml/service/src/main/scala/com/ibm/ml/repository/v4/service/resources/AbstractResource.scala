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
import akka.http.scaladsl.server.Directives
import com.ibm.ml.repository.v4.service.api.v4.ResourceAPI
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.cams.CAMSResourceMethods
import com.ibm.ml.repository.v4.service.endpoints.AbstractResourceMethods
import spray.json._

import scala.concurrent.ExecutionContext

abstract class AbstractResource[EntityRequest, Resource, Resources](sc: ServiceContext)
                                                                   (implicit system: ActorSystem,
                                                                    requestJF: RootJsonFormat[EntityRequest],
                                                                    resourceJF: RootJsonFormat[Resource],
                                                                    resourcesJF: RootJsonFormat[Resources])
  extends ResourceValidator[EntityRequest]
    with ResourceConverter[EntityRequest, Resource, Resources]
    with ResourceEndpoint
    with ResourceMethods
    with Directives {
  implicit val ec: ExecutionContext = system.dispatcher
  override val resourceMethods: AbstractResourceMethods[EntityRequest, Resource, Resources] = AbstractResourceMethods[EntityRequest, Resource, Resources](this)
  val camsResource: CAMSResourceMethods = CAMSResourceMethods(sc.camsClient, assetType)
  val resourceAPI: ResourceAPI = ResourceAPI(this)(sc)
}
