/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.utils.errors.{MLFailure, MLFailures}
import com.ibm.ml.repository.v4.utils._
import com.typesafe.config.ConfigValue

import scala.util.{Failure, Success, Try}

object MigrationConfig extends MigrationConstant {
  lazy val runtimeSoftwareSpecMap: Map[String, String] = loadConfigurationAsMap(RUNTIME_SOFTWARE_SPEC_MAPPING_PATH)

  lazy val computeHardwareSpecMap: Map[String, String] = loadConfigurationAsMap(COMPUTE_HARDWARE_SPEC_MAPPING_PATH)

  lazy val softwareSpecUpgradePublicMap: Map[String, String] = loadConfigurationAsMap(SOFTWARE_SPEC_UPGRADE_PUBLIC_MAPPING_PATH)
  lazy val softwareSpecUpgradeCommonMap: Map[String, String] = loadConfigurationAsMap(SOFTWARE_SPEC_UPGRADE_COMMON_MAPPING_PATH)
  lazy val softwareSpecUpgradePrivateMap: Map[String, String] = loadConfigurationAsMap(SOFTWARE_SPEC_UPGRADE_PRIVATE_MAPPING_PATH)

  lazy val modelTypeUpgradePublicMap: Map[String, String] = loadConfigurationAsMap(MODEL_TYPE_UPGRADE_PUBLIC_MAPPING_PATH)
  lazy val modelTypeUpgradeCommonMap: Map[String, String] = loadConfigurationAsMap(MODEL_TYPE_UPGRADE_COMMON_MAPPING_PATH)
  lazy val modelTypeUpgradePrivateMap: Map[String, String] = loadConfigurationAsMap(MODEL_TYPE_UPGRADE_PRIVATE_MAPPING_PATH)

  private def loadConfigurationAsMap(path: String) = {
    val c = Try(config.getConfig(path)) match {
      case Success(c) =>
        c
      case Failure(_) =>
        throw MLFailures(
          errors = Seq(MLFailure("invalid_configuration", s"Failed to find configuration $path")),
          trace = "unknown",
          statusCode = StatusCodes.InternalServerError.intValue
        )
    }
    var map: Map[String, String] = Map()
    val it = c.root().entrySet().iterator()
    while (it.hasNext) {
      val entry = it.next()
      entry.getValue match {
        case _: ConfigValue =>
          map = map ++ Map(entry.getKey -> c.getString("\"" + entry.getKey + "\""))
        case _ =>
          throw MLFailures(
            errors = Seq(MLFailure("invalid_configuration", s"Config value for runtime '${entry.getKey}' is not string'")),
            trace = "unknown",
            statusCode = StatusCodes.InternalServerError.intValue
          )
      }
    }
    map
  }
}
