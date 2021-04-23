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

import java.io.InputStream

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{HttpResponse, StatusCodes, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.ibm.analytics.wml.utils.assets.v4.AssetTypes
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.clients.http.{AkkaHttpClient, HttpClientBuilder, RetryPolicy}
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging._
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

case class AssetDescriptor(name: String, path: String, version: String, content: Try[String])

object GlobalAssetTypesCreator {
  // for now we force 4.0.0 - set to 'None' to use the latest versions
  // or explicitly set the version when calling 'declareGlobalAssetTypes'
  val DEFAULT_USE_VERSION: Option[String] = None //Some("4.0.0")

  /** map by asset type of (name, version, Try(content)) */
  private def assetPayloads(useVersion: Option[String]): Map[String, AssetDescriptor] = {
    /*
    def loadPath(name: String, path: String): (String, (String, Try[String])) = {
      val value = Using ( ClassLoader.getSystemClassLoader.getResourceAsStream(path)) { reader =>
        scala.io.Source.fromInputStream(reader).mkString
      }
      (name, (path, value))
    }
     */

    def loadAsset(name: String): (String, AssetDescriptor) = {
      def loadPath(path: String): Try[String] = Try {
        var reader: InputStream = null
        try {
          reader = ClassLoader.getSystemClassLoader.getResourceAsStream(path)
          scala.io.Source.fromInputStream(reader).mkString
        }
        finally {
          Try(reader.close())
        }
      }
      AssetTypes.getAssetPath(name, useVersion) match {
        case Success((path, version)) =>
          (name, AssetDescriptor(name, path, version, loadPath(path)))
        case Failure(exception) =>
          val error: Try[String] = Failure(exception)
          (name, AssetDescriptor(name, "-", "-", error))
      }
    }

    AssetTypes.mlAssetOrder.view.map(nv => loadAsset(nv)).toMap
  }
}

case class GlobalAssetTypesCreator(camsUrl: Uri,
                                   authorization: Map[String, String],
                                   listOnly: Boolean,
                                   forceUpdate: Boolean,
                                   clientLoader: HttpClientBuilder,
                                   retryPolicy: RetryPolicy) extends StrictLogging with DefaultJsonProtocol {
  private implicit val credentials: Option[HttpCredentialsProvider] = None
  private implicit val retries: RetryPolicy = retryPolicy

  private def getClient(url: Uri): Future[AkkaHttpClient] = {
    Future.fromTry {
      Try {
        val port = if (url.authority.port > 0) url.authority.port else 443
        clientLoader.get(url.authority.host.address(), port, HttpClientBuilder.Keys.CAMS_CLIENT)
      }
    }
  }

  private def getRequestId(requestId: Option[String],
                           assetName: String,
                           fallback: String): String = {
    requestId match {
      case Some(reqId) =>
        s"$reqId-$assetName"
      case None =>
        fallback
    }
  }

  private def getEntity(response: HttpResponse)
                       (implicit system: ActorSystem): Future[Option[String]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    {
      for {
        entity <- Unmarshal(response.entity).to[String]
      } yield {
        Some(entity)
      }
    } recover {
      case _: Throwable =>
        Try(response.discardEntityBytes()).recover {
          case t =>
            logger.warn(s"Unable to discard HttpResponse content: ${t.getMessage}", t)
        }
        None
    }
  }

  private def logResponse(uri: String,
                          actionName: String,
                          headers: Map[String, String],
                          requestId: Option[String],
                          response: HttpResponse): Future[Unit] = {
    Future.successful {
      reqId(requestId)(() => logger.info(s"Request $uri for $actionName with request headers ${logHTTPHeaders(headers)} returned response $response"))
    }
  }

  private def registerType(typename: String,
                           payload: JsValue,
                           requestId: Option[String])
                          (implicit system: ActorSystem): Future[HttpResponse] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val assetTypePutUrl: Uri = typename match {
      case AssetTypes.ML_SCRIPT => s"$camsUrl/v2/asset_types/script"
      case AssetTypes.ML_RSHINY => s"$camsUrl/v2/asset_types/shiny_asset"
      case _ => s"$camsUrl/v2/asset_types/$typename"
    }

    val actionName = s"register-global-asset-type-$typename"
    val rId: String = getRequestId(requestId, typename, actionName)
    for {
      _ <- Future.successful(reqId(Some(rId))(() => logger.info(s"Registering AssetType $typename"))) // with payload ${logPrint(payload)}"))
      client <- getClient(assetTypePutUrl)
      response <- client.putJson(
        uri = assetTypePutUrl.path.toString(),
        action = Some(actionName),
        headers = authorization ++ Map(HEADER_GLOBAL_TRANSACTION_ID -> rId),
        body = payload
      )
      _ <- logResponse(assetTypePutUrl.path.toString(), actionName, authorization, Some(rId), response)
    } yield {
      if (response.status.isSuccess())
        reqId(Some(rId))(() => logger.info(s"Registered asset type $typename successfully with ${response.status}"))
      response
    }
  }

  private def getTypeExists(typename: String,
                            requestId: Option[String])
                           (implicit system: ActorSystem): Future[Option[JsObject]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val assetTypeGetUrl: Uri = typename match {
      case "wml_script" => s"$camsUrl/v2/asset_types/script"
      case "wml_shiny" => s"$camsUrl/v2/asset_types/shiny_asset"
      case _ => s"$camsUrl/v2/asset_types/$typename"
    }

    val actionName = s"get-global-asset-type-$typename"
    val rId: String = getRequestId(requestId, typename, actionName)
    for {
      client <- getClient(assetTypeGetUrl)
      response <- client.get(
        uri = assetTypeGetUrl.path.toString(),
        action = Some(actionName),
        headers = authorization ++ Map(HEADER_GLOBAL_TRANSACTION_ID -> rId)
      )
      _ <- logResponse(assetTypeGetUrl.path.toString(), actionName, authorization, Some(rId), response)
      entity <- getEntity(response)
    } yield {
      (response.status, entity) match {
        case (StatusCodes.OK, Some(reply)) =>
          reqId(Some(rId))(() => logger.info(s"AssetType $typename exists"))
          Try {
            JsonParser(reply).asJsObject()
          } match {
            case Success(json) =>
              Some(json)
            case Failure(exception) =>
              val msg = s"Failed to parse response as json from get type for $typename"
              reqId(Some(rId))(() => logger.error(msg))
              throw ExecutionException(msg, Some(exception))
          }
        case (StatusCodes.OK, None) =>
          val msg: String = s"AssetType $typename exists but has no content"
          reqId(Some(rId))(() => logger.error(msg))
          throw ExecutionException(msg)
        case (StatusCodes.NotFound, _) =>
          reqId(Some(rId))(() => logger.info(s"AssetType $typename not found"))
          None
        case (StatusCodes.Unauthorized, _) | (StatusCodes.Forbidden, _) =>
          throw AuthenticationException(s"${response.status} when trying to get asset type $typename")
        case (unrecognized, Some(reply)) =>
          throw ExecutionException(s"Unexpected response $unrecognized when trying to get asset type $typename with response $reply")
        case (unrecognized, None) =>
          throw ExecutionException(s"Unexpected response $unrecognized when trying to get asset type $typename")
      }
    }
  }

  private def getTypeStatus(assetTypeStatusUrl: Uri,
                            typename: String,
                            requestId: Option[String])
                           (implicit system: ActorSystem): Future[HttpResponse] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val actionName = s"get-global-asset-type-status-$typename"
    val rId: String = getRequestId(requestId, typename, actionName)
    for {
      client <- getClient(assetTypeStatusUrl)
      response <- client.get(
        uri = assetTypeStatusUrl.path.toString(),
        action = Some(actionName),
        headers = authorization ++ Map(HEADER_GLOBAL_TRANSACTION_ID -> rId)
      )
      _ <- logResponse(assetTypeStatusUrl.path.toString(), actionName, authorization, Some(rId), response)
      _ <- {
        response.status match {
          case StatusCodes.Unauthorized | StatusCodes.Forbidden =>
            Future.failed(AuthenticationException(s"${response.status} when trying to create asset type $typename"))
          case _ =>
            Future.successful(())
        }
      }
    } yield {
      response
    }
  }

  private def waitForStatus(assetTypeStatusUrl: Uri,
                            typename: String,
                            oldState: AssetState,
                            requestId: Option[String],
                            version: String)
                           (implicit system: ActorSystem): Future[AssetStatus] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val startTime = System.currentTimeMillis()

    val maxRequestTime: Long = getDuration("register-global-asset-types.get-status.max-request-time", 24.hours).toMillis
    val delay = getDuration("register-global-asset-types.get-status.delay", 30.second)

    def isCreated(assetStatus: Option[AssetTypeStatus]): Boolean = {
      assetStatus.isDefined && assetStatus.get.status.isDefined && assetStatus.get.status.get.toUpperCase.contains("CREATED")
    }

    def doRetry(maxRequestTime: Long, count: Int): Future[AssetStatus] = {
      for {
        response <- getTypeStatus(assetTypeStatusUrl, typename, requestId)
        entity <- getEntity(response)
        status <- {
          import AssetStatus._
          val assetStatus: Option[AssetTypeStatus] = if (entity.isDefined) {
            Try(JsonParser(entity.get).convertTo[AssetTypeStatus]) match {
              case Success(as) =>
                reqId(requestId)(() => logger.debug(s"Asset ${as.name} has state ${as.status.getOrElse("unknown")} with return code ${response.status.intValue()}"))
                Some(as)
              case Failure(_) =>
                None
            }
          } else
            None
          response.status match {
            case StatusCodes.SeeOther if isCreated(assetStatus) =>
              reqId(requestId)(() => logger.info(s"AssetType created (${response.status.intValue()}) successfully for $typename with response ${entity.getOrElse("<none>")}"))
              Future.successful(AssetStatus(typename, oldState, UpToDate, version))
            case status =>
              if (response.status.isSuccess()) {
                reqId(requestId)(() => logger.info(s"Call successful with status $status but reply ${entity.getOrElse("<none>")}"))
              } else {
                reqId(requestId)(() => logger.info(s"Call failed with status $status and reply ${entity.getOrElse("<none>")}"))
              }
              val duration = System.currentTimeMillis() - startTime
              if (duration < maxRequestTime) {
                val duration = System.currentTimeMillis() - startTime
                reqId(requestId)(() => logger.info(s"HTTP RETRY (status = ${response.status}): count $count in ${formatAsDuration(duration)}"))
                val promise = Promise[AssetStatus]()
                system.scheduler.scheduleOnce(delay) {
                  promise.completeWith(doRetry(maxRequestTime, count + 1))
                }
                promise.future
              } else {
                val msg = s"HTTP RETRY aborted, request duration of ${formatAsDuration(duration)} exceeds maximum request time ${formatAsDuration(maxRequestTime)} with response ${entity.getOrElse("unknown")}"
                reqId(requestId)(() => logger.info(msg))
                Future.failed(RegistrationTimeoutException(msg, count))
              }
          }
        }
      } yield {
        status
      }
    }

    doRetry(maxRequestTime, 1) recoverWith {
      case e: RegistrationTimeoutException =>
        val duration = System.currentTimeMillis() - startTime
        reqId(requestId)(() => logger.info(s"Registration failed with timeout for count ${e.count} after ${formatAsDuration(duration)}"))
        Future.failed(e)
      case e: Throwable =>
        reqId(requestId)(() => logger.info(s"Unrecognized exception whilst trying to get asset status: ${e.getMessage}", e))
        Future.failed(e)
    }
  }

  private def parsePayload(assetPayload: String,
                           path: String,
                           requestId: Option[String])
                          (implicit system: ActorSystem): Future[JsObject] = {
    implicit val ec: ExecutionContext = system.dispatcher

    def msg(e: Throwable) = s"Failed to parse global asset payload with error ${e.getMessage} for payload $assetPayload"

    {
      for {
        payload <- Future(JsonParser(assetPayload).asJsObject)
      } yield {
        reqId(requestId)(() => logger.info(s"Parsed $path payload"))
        payload
      }
    } recoverWith {
      case e: GlobalAssetTypesException =>
        reqId(requestId)(() => logger.error(msg(e), e))
        throw e
      case e: Throwable =>
        reqId(requestId)(() => logger.error(msg(e), e))
        throw ExecutionException(msg(e), Some(e))
    }
  }

  private def getVersion(name: String,
                         payload: JsObject,
                         requestId: Option[String]): Option[String] = {
    Try {
      payload.fields("properties")
        .asJsObject.fields("ml_version")
        .asJsObject.fields("default_value")
        .asInstanceOf[JsString].value
    } match {
      case Success(version) =>
        reqId(requestId)(() => logger.info(s"Found version $version in payload for $name"))
        Some(version)
      case Failure(exception) =>
        reqId(requestId)(() => logger.warn(s"Failed to find the asset version in payload for $name: ${exception.getMessage}"))
        None
    }
  }

  private def versionChanged(name: String,
                             payload: JsObject,
                             existingPayload: Option[JsObject],
                             requestId: Option[String]): Future[(AssetState, Boolean)] = {
    val newVersion = getVersion(name, payload, requestId)
    newVersion match {
      case Some(nv) => // we have a version in the definition to be registered
        existingPayload match {
          case Some(ePayload) => // we have a valid existing payload
            val existingVersion = getVersion(name, ePayload, requestId)
            existingVersion match {
              case Some(ev) if ev == nv => // the versions match
                reqId(requestId)(() => logger.info(s"No need to register type $name because version $nv is already registered"))
                Future.successful(UpToDate, false)
              case Some(ev) if ev != nv => // the versions do not match
                reqId(requestId)(() => logger.info(s"Register type $name because existing version $ev does not match version to be registered $nv"))
                Future.successful(OldVersion, true)
              case _ => // no existing version found
                reqId(requestId)(() => logger.info(s"Register type $name with version $nv because no existing version found"))
                Future.successful(OldVersion, true)
            }
          case None => // we have no existing payload
            reqId(requestId)(() => logger.info(s"Register type $name with version $nv because existing no existing payload found"))
            Future.successful(Missing, true)
        }
      case None => // the definition to be registered does not have a version (so just check contents)
        existingPayload match {
          case Some(ePayload) =>
            if (payload.compactPrint.equals(ePayload.compactPrint)) {
              reqId(requestId)(() => logger.info(s"No need to register type $name because existing payloads are identical (json.compactPrint)"))
              Future.successful(UpToDate, false)
            } else {
              reqId(requestId)(() => logger.info(s"Register type $name because existing payload does not match new payload (json.compactPrint)"))
              Future.successful(OldVersion, true)
            }
          case None =>
            reqId(requestId)(() => logger.info(s"Register type $name because existing no existing payload found"))
            Future.successful(Missing, true)
        }
    }
  }

  private def shouldUpdateAsset(desc: AssetDescriptor,
                                payload: JsObject,
                                requestId: Option[String])
                               (implicit system: ActorSystem): Future[(AssetState, Boolean)] = {
    implicit val ec: ExecutionContext = system.dispatcher

    def msg(e: Throwable) = s"Failed to check if asset ${desc.name} ${desc.version} is registered from ${desc.path} with $camsUrl: ${e.getMessage}"

    {
      for {
        // first see if we have it already
        existingPayload <- getTypeExists(desc.name, requestId)
        (oldState, shouldUpdate) <- versionChanged(desc.name, payload, existingPayload, requestId)
      } yield {
        (oldState, shouldUpdate)
      }
    } recoverWith {
      case e: GlobalAssetTypesException =>
        reqId(requestId)(() => logger.error(msg(e), e))
        throw e
      case e: Throwable =>
        reqId(requestId)(() => logger.error(msg(e), e))
        throw ExecutionException(msg(e), Some(e))
    }
  }

  private def updateAsset(desc: AssetDescriptor,
                          requestId: Option[String])
                         (implicit system: ActorSystem): Future[AssetStatus] = {
    implicit val ec: ExecutionContext = system.dispatcher

    def msg(e: Throwable) = s"""Failed to ${if (listOnly) "list" else "update"} asset ${desc.name} ${desc.version} from ${desc.path} with $camsUrl: ${e.getMessage}"""

    {
      for {
        // check that we managed to load the path
        content <- Future.fromTry(desc.content)
        // parse the payload
        payload <- parsePayload(content, desc.path, requestId)
        // see if we should update the asset
        (oldState, shouldUpdate) <- {
          if (forceUpdate) {
            Future.successful((ForcedUpdate, true))
          } else {
            shouldUpdateAsset(desc, payload, requestId)
          }
        }
        status <- {
          // listOnly overrides forceUpdate
          if (shouldUpdate && !listOnly) {
            for {
              registerResponse <- registerType(desc.name, payload, requestId)
              statusUrl <- Future {
                registerResponse.headers.find(header => header.lowercaseName == "location") match {
                  case Some(header) =>
                    Try(Uri(header.value())) match {
                      case Success(uri) =>
                        // make sure we always have the host and port for both cpd and cloud
                        camsUrl.copy(path = uri.path, rawQueryString = uri.rawQueryString, fragment = uri.fragment)
                      case Failure(exception) =>
                        throw BadResponse(s"Failed to convert the 'Location' response header ${header.value} when registering ${desc.name}: ${exception.getMessage}", Some(exception))
                    }
                  case None =>
                    if (registerResponse.status.isSuccess()) {
                      throw BadResponse(s"Failed to find the 'Location' response header when registering ${desc.name}")
                    } else {
                      val msg = s"Request to register ${desc.name} failed with response $registerResponse"
                      logger.warn(msg)
                      throw BadResponse(msg)
                    }
                }
              } transform { res =>
                Try(registerResponse.discardEntityBytes()).recover {
                  case t =>
                    logger.warn(s"Unable to discard HttpResponse content: ${t.getMessage}", t)
                }
                res
              }
              response <- waitForStatus(statusUrl, desc.name, oldState, requestId, desc.version)
            } yield {
              response
            }
          } else
            Future.successful(AssetStatus(desc.name, oldState, oldState, desc.version))
        }
      } yield {
        status
      }
    } recoverWith {
      case e: GlobalAssetTypesException =>
        reqId(requestId)(() => logger.error(msg(e), e))
        Future.failed(e)
      case e: Throwable =>
        reqId(requestId)(() => logger.error(msg(e), e))
        Future.failed(ExecutionException(msg(e), Some(e)))
    }
  }

  /**
   * Public entry point (mainly for tests)
   *
   * @param requestId The request id.
   * @param system    The actor system for the futures.
   * @return The status of each asset.
   */
  def declareGlobalAssetTypes(requestId: Option[String],
                              useVersion: Option[String] = GlobalAssetTypesCreator.DEFAULT_USE_VERSION)
                             (implicit system: ActorSystem): Future[List[AssetStatus]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    reqId(requestId)(() => logger.info(s"Registering assets with CAMS $camsUrl"))

    Future.sequence(GlobalAssetTypesCreator.assetPayloads(useVersion).map(values => updateAsset(values._2, requestId)).toList) recoverWith {
      case e: GlobalAssetTypesException =>
        throw e
      case e: Throwable =>
        val msg = s"Failed to update global assets: ${e.getMessage}"
        reqId(requestId)(() => logger.error(msg, e))
        throw ExecutionException(msg, Some(e))
    }
  }
}

// private exceptions

private case class RegistrationTimeoutException(message: String, count: Int) extends Exception(message) {
  override def getMessage: String = {
    s"Registration timeout $message for call $count"
  }
}

private case class InternalErrorException(message: String) extends Exception(message)
