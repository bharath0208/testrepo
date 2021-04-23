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

import akka.http.scaladsl.model._
import com.ibm.analytics.wml.utils.errors.{MLFailure, MLFailures, Target}
import spray.json._

import scala.annotation.tailrec

case class ServiceException(status: Int,
                            code: String,
                            message: String,
                            moreInfo: Option[String],
                            cause: Option[Throwable],
                            downstreamFailures: Option[Seq[MLFailure]],
                            target: Option[Target]) extends Exception {
  require(code != null, "No code for service exception")
  require(message != null, "No message for service exception")

  override def getMessage: String = message

  override def getCause: Throwable = {
    cause match {
      case Some(c) => c
      case None => super.getCause
    }
  }

  def getFailures(fallbackMoreInfo: Option[String] = None): Seq[MLFailure] = {
    def getFailure: MLFailure = {
      val info = moreInfo match {
        case Some(info) => Some(info)
        case None => fallbackMoreInfo
      }
      MLFailure(code, getMessage, target, info, getCause)
    }
    downstreamFailures match {
      case Some(failures) =>
        Seq(getFailure) ++ failures
      case None =>
        Seq(getFailure)
    }
  }

  override def toString: String = {
    logPrint(
      MLFailures(
        errors = getFailures(),
        trace = "unknown",
        statusCode = status
      ).toJson
    )
  }
}

object ServiceException {
  def apply(status: StatusCode,
            codes: MessageCode,
            cause: Option[Throwable] = None,
            downstreamFailures: Option[Seq[MLFailure]] = None): ServiceException = {
    new ServiceException(status.intValue, codes.code, codes.message, codes.moreInfo, cause, downstreamFailures, codes.target)
  }

  @tailrec
  def getExceptionMessage(e: Throwable): String = {
    val msg = e.getMessage
    if ((msg == null) || msg.trim.isEmpty) {
      e match {
        case e if e.getCause != null => getExceptionMessage(e.getCause)
        case e => e.getClass.getSimpleName
      }
    } else
      msg
  }

  def getTarget(e: Throwable): Option[Target] = {
    e match {
      case de: DeserializationException if de.fieldNames.nonEmpty =>
        Some(Target("field", de.fieldNames.mkString(".")))
      case _ =>
        None
    }
  }
}

trait MessageCode {
  val code: String
  val message: String
  val target: Option[Target] = None
  val moreInfo: Option[String] = None

  def asFailure: MLFailure = {
    MLFailure(
      code,
      message,
      target,
      moreInfo
    )
  }
}

case class InvalidConfigurationMessage(message: String) extends MessageCode {
  override val code = "invalid_configuration"
}

case class InternalErrorExceptionMessage(m: String) extends MessageCode {
  override val code = "internal_error"
  override val message = s"Internal error: $m"
}

/** Failed to create the SSL configuration */
case class SSLConfigFailedMessage() extends MessageCode {
  override val code = "ssl_config_failed"
  override val message = "Failed to create the security context"
}

/** Failed to create the service */
case class MLRepositoryServiceFailed() extends MessageCode {
  override val code = "ml_repository_service_failed"
  override val message = "Failed to create the V4 repository service"
}

case class UnrecognizedActorSystemMessage(name: String) extends MessageCode {
  override val code = "unrecognized_system_name"
  override val message = s"Unrecognized actor system name $name"
}

case class NotFoundMessage(uri: String) extends MessageCode {
  override val code = "not_found"
  override val message = s"Resource $uri not found"
}

case class QueryNotFoundMessage(assetType: String, typeName: String) extends MessageCode {
  override val code = "not_found"
  override val message = s"Resource $assetType '$typeName' not found"
}

case class MethodNotAllowedMessage(requested: String, supported: String) extends MessageCode {
  override val code = "method_not_allowed"
  override val message = s"The HTTP method $requested is not supported, expected $supported."
}

case class UnknownContentTypeMessage(supportedTypes: String) extends MessageCode {
  override val code = "unsupported_content_type"
  override val message = s"The request's Content-Type is not supported, expected: $supportedTypes."
}

case class NoContentMessage() extends MessageCode {
  override val code = "no_content"
  override val message = "No content for the request"
}

case class ContentValidationFailedMessage(error: String) extends MessageCode {
  override val code = "content_validation_failed"
  override val message = s"Content validation failed: $error"
}

case class MalformedRequestContentMessage(error: String, override val target: Option[Target]) extends MessageCode {
  override val code = "malformed_request_content"
  override val message = s"Malformed request content: $error"
}

case class MissingQueryParameterMessage(parameter: String) extends MessageCode {
  override val code = "missing_query_parameter"
  override val message = s"Mandatory '$parameter' query parameter not found"
  override val target: Option[Target] = Some(Target("parameter", parameter))
}

case class MissingOneOfQueryParametersMessage(parameters: String*) extends MessageCode {
  override val code = "missing_one_of_query_parameter"
  override val message = s"Mandatory query parameter not found when expecting one (and only one) of ${parameters.mkString(", ")}"
  override val target: Option[Target] = Some(Target("parameter", s"One of ${parameters.mkString(", ")}"))
}

case class MoreThanOneOfQueryParametersMessage(parameters: String*) extends MessageCode {
  override val code = "ambiguous_query_parameter"
  override val message = s"More than one query parameter found when expecting one of ${parameters.mkString(",")}"
  override val target: Option[Target] = Some(Target("parameter", s"Only one of ${parameters.mkString(", ")}"))
}

case class InvalidQueryParameterMessage(parameter: String, value: String, reason: Option[String] = None, expectedValues: Seq[String] = Seq()) extends MessageCode {
  override val code = "invalid_query_parameter"
  override val message: String = if (reason.isDefined) {
    s"Invalid value $value for query parameter $parameter: ${reason.get}"
  } else if (expectedValues.nonEmpty) {
    s"Invalid value $value for query parameter $parameter when expecting ${expectedValues.mkString(",")}"
  } else {
    s"Invalid value $value for query parameter $parameter"
  }
  override val target: Option[Target] = Some(Target("parameter", parameter))
}

case class UnexpectedQueryParameters(parameters: Seq[String], expectedParameters: Seq[String] = Seq()) extends MessageCode {
  override val code = "unexpected_query_parameters"
  override val message: String = if (expectedParameters.isEmpty) {
    s"Invalid query parameters ${parameters.mkString(",")}"
  } else if (expectedParameters.size == 1) {
    s"Invalid query parameter ${parameters.mkString(",")} when expecting ${expectedParameters.mkString(",")}"
  } else {
    s"Invalid query parameters ${parameters.mkString(",")} when expecting ${expectedParameters.mkString(",")}"
  }
}

// this is used mainly when trying to convert json uploaded to the API
case class MissingJsonDocumentMessage() extends MessageCode {
  override val code = "missing_json_document"
  override val message = "Missing json document"
}

// this is used mainly when trying to convert json uploaded to the API
case class MalformedJsonDocumentMessage(e: Throwable, documentType: String = "json document") extends MessageCode {
  override val code = "malformed_json"
  override val message = s"Malformed $documentType: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class NoAttachmentAvailableMessage() extends MessageCode {
  override val code = "attachment_not_available"
  override val message = s"There's no available attachment for the targeted asset"
}

case class TooManyAttachmentAvailableMessage() extends MessageCode {
  override val code = "attachment_not_available"
  override val message = s"More than one attachments meet the criteria, please download the content by id"
}

case class InvalidRequestEntityMessage(msg: String) extends MessageCode {
  override val code = "invalid_request_entity"
  override val message = s"Invalid request entity: $msg"
}

case class FailedConvertCAMSResources(e: Throwable) extends MessageCode {
  override val code = "failed_convert_CAMS_resources"
  override val message = s"Failed to convert CAMS resources due to an error: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedConvertMigrationResources(e: Throwable) extends MessageCode {
  override val code = "failed_convert_migration_resources"
  override val message = s"Failed to convert migration resources due to an error: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedConvertCAMSResource(e: Throwable) extends MessageCode {
  override val code = "failed_convert_CAMS_resource"
  override val message = s"Failed to convert CAMS resource due to an error: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedConvertCAMSResourceRevisions(e: Throwable) extends MessageCode {
  override val code = "failed_convert_CAMS_resource_revisions"
  override val message = s"Failed to convert CAMS resource revisions due to an error: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedConvertCAMSResourceContent(e: Throwable) extends MessageCode {
  override val code = "failed_convert_CAMS_resource_content"
  override val message = s"Failed to convert CAMS resource content due to an error: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedConvertEntityToCAMS(e: Throwable) extends MessageCode {
  override val code = "failed_convert_entity_to_CAMS"
  override val message = s"Failed to convert entity to CAMS resources due to an error: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedToGetServiceId() extends MessageCode {
  override val code = "failed_to_get_service_id"
  override val message = s"Not able to use service id as not configured"
}

case class FailedToGetAsset(id: String, assetType: String, msg: String) extends MessageCode {
  override val code = "failed_to_get_asset"
  override val message = s"Failed to get $assetType asset $id: $msg"
}

case class FailedToGetDependencies(msg: String) extends MessageCode {
  override val code = "failed_to_get_dependencies"
  override val message = s"Failed to get the dependencies: $msg"
}

case class FailedToCreateNewAsset(msg: String) extends MessageCode {
  override val code = "failed_to_create_new_asset"
  override val message = s"Failed to create new asset : $msg"
}

case class FailedToConvertToRequest(e: Throwable) extends MessageCode {
  override val code = "failed_to_convert_to_new_asset_request"
  override val message = s"Failed to convert to new asset request payload: ${ServiceException.getExceptionMessage(e)}"
  override val target: Option[Target] = ServiceException.getTarget(e)
}

case class FailedToMigrateDependencies(msg: String) extends MessageCode {
  override val code = "failed_to_migrate_dependencies"
  override val message = s"Failed to migrate dependencies: $msg"
}

case class FailedToMigrateAsset(id: String, assetType: String, msg: String) extends MessageCode {
  override val code = "failed_to_migrate_asset"
  override val message = s"Failed to migrate $assetType asset '$id': $msg"
}

case class MissingRequiredEnvironmentVariable(name: String) extends MessageCode {
  override val code = "missing_required_environment_variable"
  override val message = s"Missing required environment variable: $name"
}

case class FailedToConvertMigrationJob(json: String) extends MessageCode {
  override val code = "failed_convert_migration_job"
  override val message = s"Failed to read the json: $json"
}

case class AssetTypeMismatch(requestAssetType: String, id: String, actualAssetType: String) extends MessageCode {
  override val code = "asset_type_mismatch"
  override val message = s"Invalid request asset type $requestAssetType. The asset type of the asset id $id is $actualAssetType"
}

case class JobAccessForbidden(id: String) extends MessageCode {
  override val code = "job_access_forbidden"
  override val message = s"You aren't authorized to acces the job id $id"
}

case class JobAlreadyTerminated(id: String) extends MessageCode {
  override val code = "job_already_terminated"
  override val message = s"Job id $id already terminated"
}

// generic message when we detect a downstream error
case class DownstreamError(code: String = "downstream_error", status: Int) extends MessageCode {
  override val message: String = {
    if ((status == 404) || (status == 410))
      "The requested asset does not exist in the downstream service"
    else
      "The service is experiencing some downstream errors, please re-try the request"
  }
}

case class FailedToUpdateAsset(msg: String) extends MessageCode {
  override val code = "failed_asset_update"
  override val message = s"Failed to update the asset: $msg"
}

case class QuiesceModeEnabled() extends MessageCode {
  override val code = "quiesce_mode_enabled"
  override val message = s"This service is in 'quiesce' mode which means that only read only actions are permitted"
}

case class ContentTypeNotAllowedMessage(contentType: String, supported: String) extends MessageCode {
  override val code = "content_type_not_allowed"
  override val message = s"Content type $contentType is not allowed, expected $supported."
}

case class ContentTypeNotAllowedForModelTypeMessage(contentType: String, modelType: String, supported: String) extends MessageCode {
  override val code = "content_type_not_allowed_for_model_type"
  override val message = s"Content type $contentType is not allowed for model type $modelType, expected $supported."
}

case class ContentTypeMissingMessage(msg: String) extends MessageCode {
  override val code = "content_type_missing"
  override val message = s"Upload content failed due to error: $msg"
}

