/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.cams

import com.ibm.ml.repository.v4.utils.logPrint
import spray.json._

case class AssetTypeStatus(name: Option[String],
                           status: Option[String])

sealed trait AssetState {
  val name: String
}

case object Missing extends AssetState {
  override val name: String = "missing"
}

case object OldVersion extends AssetState {
  override val name: String = "old version"
}

case object ForcedUpdate extends AssetState {
  override val name: String = "forced update"
}

case object UpToDate extends AssetState {
  override val name: String = "up to date"
}

case class AssetStatus(name: String,
                       oldState: AssetState,
                       newState: AssetState,
                       newVersion: String)

object AssetStatus extends DefaultJsonProtocol {
  implicit val assetTypeStatusFormat: RootJsonFormat[AssetTypeStatus] = jsonFormat2(AssetTypeStatus.apply)

  implicit val assetStateFormat: RootJsonFormat[AssetState] = new RootJsonFormat[AssetState]() {
    override def read(json: JsValue): AssetState = {
      json match {
        case JsString(name) =>
          name match {
            case Missing.name => Missing
            case OldVersion.name => OldVersion
            case UpToDate.name => UpToDate
            case _ =>
              deserializationError(s"Unrecognized asset state: $name")
          }
        case _ =>
          deserializationError(s"Unrecognized json for asset state when expecting one of [${Missing.name}, ${OldVersion.name}, ${UpToDate.name}]: ${logPrint(json)}")
      }
    }

    override def write(assetState: AssetState): JsValue = {
      JsString(assetState.name)
    }
  }

  implicit val assetStatusFormat: RootJsonFormat[AssetStatus] = jsonFormat(AssetStatus.apply,
    "name",
    "old_state",
    "new_state",
    "new_version"
  )
}
