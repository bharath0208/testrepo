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

import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common.{CommonRevisionRequest, PatchPayloadElement}
import com.ibm.ml.repository.v4.service.utils.ValidateUtils
import com.ibm.ml.repository.v4.utils.CallContext
import spray.json._

import scala.concurrent.Future

trait ResourceValidator[EntityRequest] extends ValidateUtils {
  def validateEntity(entity: EntityRequest)
                    (implicit callContext: CallContext): Future[EntityRequest]

  def validatePatchEntity(entity: List[PatchPayloadElement])
                         (implicit callContext: CallContext): Future[JsValue] = {
    val patchEntity = entityValidatePatchPayloadByPathName(entity)
    Future.successful(patchEntity.toJson)
  }

  def validateRevisionEntity(entity: CommonRevisionRequest)
                            (implicit callContext: CallContext): Future[CommonRevisionRequest] = {
    val (trimSpaceId, trimProjectId) = entitySpaceIdOrProjectIdValidator(entity.spaceId, entity.projectId)
    Future.successful(entity.copy(spaceId = trimSpaceId, projectId = trimProjectId))
  }
}
