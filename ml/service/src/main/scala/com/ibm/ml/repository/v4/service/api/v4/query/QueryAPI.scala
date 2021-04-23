/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.api.v4.query

import akka.http.scaladsl.model.MediaTypes.`application/json`
import akka.http.scaladsl.model.{HttpEntity, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.{Directives, Route}
import akka.util.ByteString
import com.ibm.analytics.wml.api.v4ga.query.function_types.FunctionTypesJsonFormat._
import com.ibm.analytics.wml.api.v4ga.query.model_types.ModelTypesJsonFormat._
import com.ibm.analytics.wml.api.v4ga.query.{function_types, model_definitions_frameworks, model_types, script_languages}
import com.ibm.analytics.wml.utils.functions.FunctionTypeJsonFormat.functionTypeEntityFormat
import com.ibm.analytics.wml.utils.functions.{FunctionTypeEntity, FunctionTypes}
import com.ibm.analytics.wml.utils.model_definitions.ModelDefinitionFrameworkJsonFormat.modelDefTypeEntityFormat
import com.ibm.analytics.wml.utils.model_definitions.{ModelDefinitionFrameworkEntity, ModelDefinitionFrameworks}
import com.ibm.analytics.wml.utils.models.ModelTypeJsonFormat.modelTypeEntityFormat
import com.ibm.analytics.wml.utils.models.{ModelTypeEntity, ModelTypes}
import com.ibm.analytics.wml.utils.queries.{PrivateCloud, PublicCloud}
import com.ibm.analytics.wml.utils.scripts.ScriptLanguageJsonFormat.scriptTypeEntityFormat
import com.ibm.analytics.wml.utils.scripts.{ScriptLanguageEntity, ScriptLanguages}
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.ibm.ml.repository.v4.utils.{APIUtils, QueryNotFoundMessage, ServiceException, isPublicCloud}
import spray.json._

import scala.util.{Failure, Success, Try}

case class QueryAPI(sc: ServiceContext) extends APIUtils with Directives {
  /* The endpoints for the service version API. */
  val endpoints: Route = {
    respondWithDefaultHeaders(getNoCacheResponseHeaders) {
      getModelTypes ~
        getModelType ~
        getFunctionTypes ~
        getFunctionType ~
        getScriptLanguages ~
        getScriptLanguage ~
        getModelDefinitionFrameworks ~
        getModelDefinitionFramework
    }
  }

  private val environment = if (isPublicCloud) PublicCloud else PrivateCloud

  private def getOKHttpResponse(results: JsValue) = {
    HttpResponse(
      status = StatusCodes.OK,
      entity = HttpEntity(`application/json`, ByteString(results.prettyPrint))
    )
  }

  private def queryNotFound(assetType: String, typeName: String) = {
    throw ServiceException(
      StatusCodes.NotFound,
      QueryNotFoundMessage(assetType, typeName)
    )
  }

  private def handleHttpResponse(res: Try[JsValue]) = {
    res match {
      case Success(types) =>
        complete(getOKHttpResponse(types))
      case Failure(exception) =>
        failWith(exception)
    }
  }

  // /ml/v4/query/model_types
  def getModelTypes: Route = {
    path("ml" / "v4" / "query" / "model_types") {
      get {
        parameters(
          QUERY_FRAMEWORK.?,
          QUERY_SOFTWARE_SPEC.?
        ) { (framework: Option[String], softwareSpec: Option[String]) =>

          val results: Try[JsValue] = Try {
            val modelTypes: Vector[ModelTypeEntity] = ModelTypes.getTypesByEnv(environment)
              .filter(mt => framework.forall(_ == mt.framework))
              .filter(mt => softwareSpec.forall { _ =>
                ModelTypes.isBaseSoftwareSpecValid(mt.modelType, softwareSpec, softwareSpec, environment)
              })
            model_types.ModelTypes(modelTypes.length, modelTypes).toJson
          }
          handleHttpResponse(results)
        }
      }
    }
  }

  def getModelType: Route = {
    path("ml" / "v4" / "query" / "model_types" / Segment) { typeName =>
      get {
        val result: Try[JsValue] = Try {
          ModelTypes.getTypeEntityOpt(typeName, environment).getOrElse(
            queryNotFound("model type", typeName)
          ).toJson
        }
        handleHttpResponse(result)
      }
    }
  }

  // /ml/v4/query/function_types
  def getFunctionTypes: Route = {
    path("ml" / "v4" / "query" / "function_types") {
      get {
        parameters(
          QUERY_SOFTWARE_SPEC.?
        ) { (softwareSpec: Option[String]) =>
          val results: Try[JsValue] = Try {
            val functionTypes: Vector[FunctionTypeEntity] = FunctionTypes.getTypesByEnv(environment)
              .filter(mt => softwareSpec.forall { _ =>
                ModelTypes.isBaseSoftwareSpecValid(mt.functionType, softwareSpec, softwareSpec, environment)
              })
            function_types.FunctionTypes(functionTypes.length, functionTypes).toJson
          }
          handleHttpResponse(results)
        }
      }
    }
  }

  def getFunctionType: Route = {
    path("ml" / "v4" / "query" / "function_types" / Segment) { typeName =>
      get {
        val result: Try[JsValue] = Try {
          FunctionTypes.getTypeEntityOpt(typeName, environment).getOrElse(
            queryNotFound("function type", typeName)
          ).toJson
        }
        handleHttpResponse(result)
      }
    }
  }

  // /ml/v4/query/script_languages
  def getScriptLanguages: Route = {
    path("ml" / "v4" / "query" / "script_languages") {
      get {
        parameters(
          QUERY_SOFTWARE_SPEC.?
        ) { (softwareSpec: Option[String]) =>
          val results: Try[JsValue] = Try {
            val scriptLanguages: Vector[ScriptLanguageEntity] = ScriptLanguages.getLanguagesByEnv(environment)
              .filter(mt => softwareSpec.forall { _ =>
                ModelTypes.isBaseSoftwareSpecValid(mt.language, softwareSpec, softwareSpec, environment)
              })
            import com.ibm.analytics.wml.api.v4ga.query.script_languages.ScriptLanguagesJsonFormat._
            script_languages.ScriptLanguages(scriptLanguages.length, scriptLanguages).toJson
          }
          handleHttpResponse(results)
        }
      }
    }
  }

  def getScriptLanguage: Route = {
    path("ml" / "v4" / "query" / "script_languages" / Segment) { typeName =>
      get {
        val result: Try[JsValue] = Try {
          ScriptLanguages.getLanguageEntityOpt(typeName, environment).getOrElse(
            queryNotFound("script language", typeName)
          ).toJson
        }
        handleHttpResponse(result)
      }
    }
  }

  // /ml/v4/query/model_definition_frameworks
  def getModelDefinitionFrameworks: Route = {
    path("ml" / "v4" / "query" / "model_definition_frameworks") {
      get {
        parameters(
          QUERY_SOFTWARE_SPEC.?
        ) { (softwareSpec: Option[String]) =>
          val results: Try[JsValue] = Try {
            val modelDefinitionFrameworks: Vector[ModelDefinitionFrameworkEntity] = ModelDefinitionFrameworks.getTypesByEnv(environment)
              .filter(mt => softwareSpec.forall { _ =>
                ModelDefinitionFrameworks.isBaseSoftwareSpecValid(mt.framework, softwareSpec, softwareSpec, environment)
              })
            import com.ibm.analytics.wml.api.v4ga.query.model_definitions_frameworks.ModelDefinitionFrameworksJsonFormat._
            model_definitions_frameworks.ModelDefinitionFrameworks(modelDefinitionFrameworks.length, modelDefinitionFrameworks).toJson
          }
          handleHttpResponse(results)
        }
      }
    }
  }

  def getModelDefinitionFramework: Route = {
    path("ml" / "v4" / "query" / "model_definition_frameworks" / Segment) { typeName =>
      get {
        val result: Try[JsValue] = Try {
          ModelDefinitionFrameworks.getFrameworkEntityOpt(typeName, environment).getOrElse(
            queryNotFound("model definition framework", typeName)
          ).toJson
        }
        handleHttpResponse(result)
      }
    }
  }
}
