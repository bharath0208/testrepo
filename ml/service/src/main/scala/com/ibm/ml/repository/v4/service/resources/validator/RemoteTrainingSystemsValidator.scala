/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.resources.validator

import com.ibm.analytics.wml.api.v4ga.remote_training_systems.{AllowedIdentity, RemoteOrganization}
import com.ibm.ml.repository.v4.service.utils.ValidateUtils

trait RemoteTrainingSystemsValidator extends ValidateUtils {

  def validateOrganization(organization: Option[RemoteOrganization]): Option[RemoteOrganization] = {
    organization.map { org =>
      RemoteOrganization(stringFieldValidator(org.name, "organization.name"),
        org.region.map(stringFieldValidator(_, "organization.region"))
      )
    }
  }

  def validateAllowedIdentities(allowedIdentities: Vector[AllowedIdentity]): Vector[AllowedIdentity] = {
    allowedIdentities.map { id =>
      AllowedIdentity(stringFieldValidator(id.id, "allowed_identities[].id"),
        id.aiType
      )
    }
  }

  def validateRemoteAdmin(allowedIdentity: Option[AllowedIdentity]) = {
    allowedIdentity.map(id => {
      AllowedIdentity(stringFieldValidator(id.id, "remote_admin.id"),
        id.aiType
      )
    })
  }

}
