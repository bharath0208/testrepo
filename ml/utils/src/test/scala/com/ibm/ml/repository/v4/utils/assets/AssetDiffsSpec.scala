/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.assets

import java.io.{File, FileWriter, InputStream, Writer}
import java.nio.charset.StandardCharsets

import com.ibm.analytics.cams.api.v2.assets.AssetType
import com.ibm.analytics.wml.utils.assets.v4.AssetTypes
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json.{JsArray, JsBoolean, JsObject, JsString, JsValue, JsonParser}

import scala.util.{Failure, Success, Try, Using}

class AssetDiffsSpec extends AnyFeatureSpec with StrictLogging {
  /*
  private def loadPath(path: String): String = {
    val value: Try[String] = Using ( ClassLoader.getSystemClassLoader.getResourceAsStream(path)) { reader =>
      scala.io.Source.fromInputStream(reader).mkString
    }
    value match {
      case Success(json) =>
        info(s"Loaded path: $path")
        json
      case Failure(exception) =>
        fail(s"Failed to load path: $path", exception)
    }
  }
   */

  private def loadPath(path: String): String = {
    val value: Try[String] = Try {
      var reader: InputStream = null
      try {
        reader = ClassLoader.getSystemClassLoader.getResourceAsStream(path)
        scala.io.Source.fromInputStream(reader).mkString
      }
      finally {
        Try(reader.close())
      }
    }
    value match {
      case Success(json) =>
        info(s"Loaded path: $path")
        json
      case Failure(exception) =>
        fail(s"Failed to load path: $path", exception)
    }
  }

  /*
  private def saveFile(file: File, contents: String): Unit = {
    Using ( new FileWriter(file, StandardCharsets.UTF_8)) { writer =>
      writer.write(contents)
    }
  }
   */

  private def saveFile(file: File, contents: String): Unit = Try {
    var writer: Writer = null
    try {
      writer = new FileWriter(file)
      writer.write(contents)
    }
    finally {
      Try(writer.close())
    }
  }

  private def getPath(path: String, name: String): String = {
    if ((path == null) || path.trim.isEmpty)
      name
    else
      s"$path.$name"
  }

  private def changed(v4ga: JsValue, cpd30: JsValue, path: String): Option[JsValue] = {
    v4ga match {
      case v4: JsObject if cpd30.isInstanceOf[JsObject] => getDiffs(v4, cpd30.asInstanceOf[JsObject], path) match {
        case js: JsObject if js.fields.nonEmpty => Some(js)
        case _ => None
      }
      case v4: JsArray if cpd30.isInstanceOf[JsArray] && (path.endsWith("fields") || path.endsWith("relationships")) =>
        getDiffs(v4, cpd30.asInstanceOf[JsArray], path, "key") match {
          case js: JsObject if js.fields.nonEmpty => Some(js)
          case _ => None
        }
      case v4: JsArray if cpd30.isInstanceOf[JsArray] =>
        getDiffs(v4, cpd30.asInstanceOf[JsArray], path) match {
          case js: JsObject if js.fields.nonEmpty => Some(js)
          case _ => None
        }
      case _: JsObject if cpd30.isInstanceOf[JsString] && (path.contains("ml_federated_metrics") || path.contains("ml_metrics")) =>
        val msg = s"Ignoring differences for $path"
        logger.warn(msg)
        Some(JsString(msg))
      case v4: JsBoolean if cpd30.isInstanceOf[JsBoolean] && (v4 != cpd30) =>
        Some(JsString(s"$cpd30 -> $v4ga"))
      case _ if !v4ga.getClass.equals(cpd30.getClass) =>
        fail(s"Path $path found elements ${v4ga.getClass} and ${cpd30.getClass}:\nV4: ${v4ga.prettyPrint}\nCPD3: ${cpd30.prettyPrint}")
      case _ =>
        if (v4ga != cpd30)
          Some(JsString(s"$cpd30 -> $v4ga"))
        else
          None
    }
  }

  private def isMatch(obj: JsObject, keyField: String, keyValue: JsValue): Boolean = {
    obj.fields.get(keyField) match {
      case Some(oldKey) if keyValue == oldKey => true
      case _ => false
    }
  }

  private def getString(js: JsValue): String = {
    js match {
      case s: JsString => s.value
      case _ => fail(s"Found ${js.prettyPrint} when expecting a string")
    }
  }

  private def getDiffs(v4ga: JsArray,
                       cpd30: JsArray,
                       path: String,
                       keyField: String): JsObject = {
    var addedFields: JsObject = JsObject()
    var removedFields: JsObject = JsObject()

    // arrays can be in any order
    for (element <- v4ga.elements) {
      val key = element.asJsObject.fields(keyField)
      for (old <- cpd30.elements) {
        if (isMatch(old.asJsObject, keyField, key)) {
          getDiffs(element.asJsObject, old.asJsObject, getPath(path, getString(key)))
        } else {
          addedFields = JsObject(addedFields.fields ++ Map("field" -> key))
        }
      }
    }
    for (old <- cpd30.elements) {
      val key = old.asJsObject.fields(keyField)
      for (newElement <- v4ga.elements) {
        if (!isMatch(newElement.asJsObject, keyField, key))
          removedFields = JsObject(removedFields.fields ++ Map("field" -> key))
      }
    }

    var res = JsObject()
    if (addedFields.fields.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "added") -> addedFields))
    if (removedFields.fields.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "removed") -> removedFields))
    res
  }

  private def getDiffs(v4ga: JsArray,
                       cpd30: JsArray,
                       path: String): JsObject = {
    var addedFields: JsArray = JsArray()
    var removedFields: JsArray = JsArray()

    // arrays can be in any order
    for (element <- v4ga.elements) {
      if (!element.isInstanceOf[JsString])
        fail(s"Not an array of strings for path $path: ${element.prettyPrint}")
      val value: JsString = element.asInstanceOf[JsString]
      if (!cpd30.elements.contains(value))
        addedFields = JsArray(addedFields.elements ++ Vector(value))
    }
    for (old <- cpd30.elements) {
      val oldValue: JsString = old.asInstanceOf[JsString]
      if (!v4ga.elements.contains(oldValue))
        removedFields = JsArray(removedFields.elements ++ Vector(oldValue))
    }

    var res = JsObject()
    if (addedFields.elements.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "added") -> addedFields))
    if (removedFields.elements.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "removed") -> removedFields))
    res
  }

  private def getDiffs(v4ga: JsObject, cpd30: JsObject, path: String = ""): JsValue = {
    var addedFields: JsArray = JsArray()
    var removedFields: JsArray = JsArray()
    var changedFields: JsObject = JsObject()

    for (element <- v4ga.fields) {
      val old = cpd30.fields.get(element._1)
      if (old.isEmpty)
        addedFields = JsArray(addedFields.elements ++ Vector(JsString(element._1)))
      else {
        changed(element._2, old.get, getPath(path, element._1)) match {
          case Some(changed) => changedFields = JsObject(changedFields.fields ++ Map(element._1 -> changed))
          case None => // ok
        }
      }
    }
    for (element <- cpd30.fields) {
      val newField = v4ga.fields.get(element._1)
      if (newField.isEmpty)
        removedFields = JsArray(removedFields.elements ++ Vector(JsString(element._1)))
    }

    var res = JsObject()
    if (addedFields.elements.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "added") -> addedFields))
    if (removedFields.elements.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "removed") -> removedFields))
    if (changedFields.fields.nonEmpty)
      res = JsObject(res.fields ++ Map(getPath(path, "changed") -> changedFields))
    res
  }

  private def showJsonDiffs(name: String,
                            v4ga: Try[(String, String)],
                            cpd30: String,
                            file: Option[File]): Unit = {
    val v4gaJson: JsObject = v4ga match {
      case Success(v4) =>
        JsonParser(loadPath (v4._1)).asJsObject
      case Failure(exception) =>
        fail(s"Failed to load asset $name : ${exception.getMessage}", exception)
    }
    val cpd30Json: JsObject = JsonParser(loadPath(cpd30)).asJsObject
    val diffs = getDiffs(v4gaJson, cpd30Json).prettyPrint
    if (file.isDefined) {
      saveFile(file.get, diffs)
    }
    logger.info(s"$name:\n$diffs")
  }

  Feature("Assets Diffs") {
    val outputDir = new File("target/asset-diffs")
    outputDir.mkdirs()
    Scenario("Functions") {
      showJsonDiffs(
        "Functions",
        AssetTypes.getAssetPath(AssetTypes.ML_FUNCTION),
        "asset_types/wml_function.json",
        Some(new File(outputDir, "functions.json"))
      )
    }
    Scenario("Experiments") {
      showJsonDiffs(
        "Experiments",
        AssetTypes.getAssetPath(AssetTypes.ML_EXPERIMENT),
        "asset_types/wml_experiment.json",
        Some(new File(outputDir, "experiments.json"))
      )
    }
    Scenario("Model Definitions") {
      showJsonDiffs(
        "Model Definitions",
        AssetTypes.getAssetPath(AssetTypes.ML_MODEL_DEFINITION),
        "asset_types/wml_model_definition.json",
        Some(new File(outputDir, "model-definitions.json"))
      )
    }
    Scenario("Models") {
      showJsonDiffs(
        "Models",
        AssetTypes.getAssetPath(AssetTypes.ML_MODEL),
        "asset_types/wml_model.json",
        Some(new File(outputDir, "models.json"))
      )
    }
    Scenario("Pipelines") {
      showJsonDiffs(
        "Pipelines",
        AssetTypes.getAssetPath(AssetTypes.ML_PIPELINE),
        "asset_types/wml_pipeline.json",
        Some(new File(outputDir, "pipelines.json"))
      )
    }
    Scenario("Deployment Job Definitions") {
      showJsonDiffs(
        "Deployment Job Definitions",
        AssetTypes.getAssetPath(AssetTypes.ML_DEPLOYMENT_JOB_DEFINITION),
        "asset_types/wml_deployment.json",
        Some(new File(outputDir, "deployment-job-definitions.json"))
      )
    }
  }
}
