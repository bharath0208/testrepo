/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.reporting

import akka.http.scaladsl.model.StatusCodes
import com.ibm.ml.repository.v4.utils
import com.ibm.ml.repository.v4.utils.{InvalidConfigurationMessage, ServiceException}

import scala.util.Try

case class ActivityTrackerConfiguration(serviceProviderName: String,
                                        serviceProviderType: String,
                                        serviceName: String,
                                        serviceProviderRegion: String) {
  def crn(accountId: String,
          assetType: String,
          assetId: String): String = s"crn:v1:$serviceProviderName:$serviceProviderType:$serviceName:$serviceProviderRegion:a/$accountId::$assetType:$assetId"
}

object ActivityTrackerConfiguration {
  private def getConfig(configName: String): String = {
    Try(utils.config.getString(s"service.activity-tracker.$configName")).getOrElse(throw ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage(s"Failed to find '$configName' for Activity Tracker")))
  }

  def apply(): ActivityTrackerConfiguration = {
    ActivityTrackerConfiguration(
      serviceProviderName = getConfig("serviceProviderName"),
      serviceProviderType = getConfig("serviceProviderType"),
      serviceName = getConfig("serviceName"),
      serviceProviderRegion = getConfig("serviceProviderRegion")
    )
  }
}
