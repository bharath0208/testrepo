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

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.wml.utils.errors.MLFailure
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Try

class ServiceExceptionSpec extends AnyWordSpec with StrictLogging {
  var registry = Map.empty[String, MessageCode]

  def register(obj: MessageCode): obj.type = {
    val key: String = obj.code
    require(!registry.contains(key), s"ObjectRegistry for ${obj.getClass.getSimpleName} already contains ${registry(key).getClass.getSimpleName} for $key")
    registry = registry.updated(key, obj)
    obj
  }

  def getMessageCodes: Iterable[MessageCode] = registry.values

  "ServiceException" when {
    "creating exceptions" should {
      "provide exception details" in {
        object test extends MessageCode {
          override val code: String = "code"
          override val message: String = "message"
        }
        assert(test.asFailure.code == "code")
        val se = ServiceException(StatusCodes.InternalServerError, NotFoundMessage("??"))
        val e = ServiceException(StatusCodes.InternalServerError, NotFoundMessage("??"), Some(se))
        assert(e.getCause != null)
        assert(e.getMessage != null)
        assert(e.message != null)
        assert(e.toString != null)
        assert(e.moreInfo.isEmpty)
        assert(e.code != null)
        assert(e.status == StatusCodes.InternalServerError.intValue)
        ServiceException(StatusCodes.InternalServerError, NotFoundMessage("??"))
        ServiceException(StatusCodes.InternalServerError, NotFoundMessage("??"), Some(se))
        assert(Try(ServiceException(0, null, "message", None, None, None, None)).isFailure)
        assert(Try(ServiceException(0, "code", null, None, None, None, None)).isFailure)
        val e1 = new Exception()
        val e2 = new Exception(e1)
        assert(ServiceException(0, "code", "message", None, Some(e2), None, None).getCause != null)
        assert(ServiceException(0, "code", "message", None, None, None, None).getCause == null)
        val failures = ServiceException(0, "code", "message", None, None, Some(Seq(MLFailure("code", "message"))), None).getFailures()
        assert(failures.nonEmpty)
        assert(failures.size == 2)
        assert(ServiceException.getExceptionMessage(new Exception()) == classOf[Exception].getSimpleName)

        register(InvalidConfigurationMessage("??"))
        register(InternalErrorExceptionMessage("??"))
        register(SSLConfigFailedMessage())
        register(MLRepositoryServiceFailed())
        register(UnrecognizedActorSystemMessage("??"))
        register(NotFoundMessage("??"))
        register(MethodNotAllowedMessage("??", "??"))
        register(UnknownContentTypeMessage("??"))
        register(NoContentMessage())
        register(ContentValidationFailedMessage("??"))
        register(MalformedRequestContentMessage("??", None))
        register(MissingQueryParameterMessage("??"))
        register(MissingOneOfQueryParametersMessage("??"))
        register(MoreThanOneOfQueryParametersMessage("??"))
        register(InvalidQueryParameterMessage("??", "??"))
        InvalidQueryParameterMessage("??", "??", Some("??"))
        InvalidQueryParameterMessage("??", "??", Some("??"), Seq("??"))
        InvalidQueryParameterMessage("??", "??", None, Seq("??"))
        register(UnexpectedQueryParameters(Seq("??")))
        UnexpectedQueryParameters(Seq("??"), Seq("s1"))
        UnexpectedQueryParameters(Seq("??"), Seq("s1", "s2"))
        register(MissingJsonDocumentMessage())
        register(MalformedJsonDocumentMessage(new Exception("??")))
        register(NoAttachmentAvailableMessage())
        register(InvalidRequestEntityMessage("??"))
        register(FailedConvertCAMSResources(new Exception("??")))
        register(FailedConvertCAMSResource(new Exception("??")))
        register(FailedConvertCAMSResourceRevisions(new Exception("??")))
        register(FailedConvertEntityToCAMS(new Exception("??")))
      }
    }
  }
}
