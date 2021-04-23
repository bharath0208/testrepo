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
import com.ibm.ml.repository.v4.service.api.v4.ContentAPI
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.service.cams.CAMSContentMethods
import com.ibm.ml.repository.v4.service.endpoints.AbstractContentMethods
import spray.json._

abstract class AbstractResourceWithContent[EntityRequest, Resource, Resources]
(sc: ServiceContext)
(implicit system: ActorSystem,
 requestJF: RootJsonFormat[EntityRequest],
 resourceJF: RootJsonFormat[Resource],
 resourcesJF: RootJsonFormat[Resources])
  extends AbstractResource[EntityRequest, Resource, Resources](sc)
    with ContentValidator
    with ContentConverter {
  val camsContent: CAMSContentMethods = CAMSContentMethods(sc.camsClient)
  override val contentMethods: Option[AbstractContentMethods[EntityRequest, Resource, Resources]] =
    Some(AbstractContentMethods[EntityRequest, Resource, Resources](this))
  val contentAPI: ContentAPI = ContentAPI(this)(sc)
}
