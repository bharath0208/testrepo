/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils

import com.ibm.analytics.wml.api.v4ga.common.VersionDate
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.containers.Container

case class CallContext(identity: Identity,
                       versionDate: VersionDate,
                       container: Container,
                       containerStorageType: Option[String]) {
  def getClientIp: Option[String] = {
    identity.clientIP match {
      case Some(remoteAddress) if remoteAddress.toIP.isDefined && !remoteAddress.toIP.get.isUnknown() =>
        Some(remoteAddress.toIP.get.ip.getHostAddress)
      case _ =>
        None
    }
  }
}
