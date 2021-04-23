/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4

import java.net.URL
import java.nio.charset.StandardCharsets
import java.text.{DateFormat, SimpleDateFormat}
import java.time.format.DateTimeFormatter
import java.time.{Duration, Instant}
import java.util.concurrent.TimeUnit
import java.util.{Base64, Date, SimpleTimeZone, UUID}

import akka.http.scaladsl.model.headers.{Authorization, RawHeader}
import akka.http.scaladsl.model.{HttpHeader, HttpMessage, HttpRequest, StatusCodes}
import com.ibm.analytics.wml.api.v4ga.common.VersionDate
import com.ibm.analytics.wml.service.utils.security.iam.IAMStableProjectsServiceId
import com.ibm.analytics.wml.service.utils.security.{Environment, StableServiceId}
import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}
import com.ibm.analytics.wml.utils.json.JsonValuesMasking
import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.scalalogging.StrictLogging
import spray.json._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

package object utils extends StrictLogging {
  // we do this first to be sure that we have it
  implicit val config: Config = ConfigFactory.load()

  def isPublicCloud: Boolean = Environment.isPublicCloud

  def isPrivateCloud: Boolean = Environment.isOnPrem

  def isDevelopment: Boolean = {
    sys.env.getOrElse("DEVELOPMENT", "false").equalsIgnoreCase("true")
  }

  val enableContentCheck : Boolean = {
    Try (config.getBoolean("service.ml-repository.enableContentCheck")).getOrElse(false)
  }

  def environment: String = sys.env.getOrElse("DEPLOYMENT_PLATFORM", "public")

  def wmlEnv: String = sys.env.getOrElse("WMLENV", "fvt")

  def dataCenter: String = sys.env.getOrElse("DATACENTRE", "Dallas")

  sealed trait AuthType {
    def name: String
  }

  case object AUTH_IAM extends AuthType {
    override def name: String = "IAM"
  }

  case object AUTH_ICP extends AuthType {
    override def name: String = "ICP"
  }

  case object AUTH_NONE extends AuthType {
    override def name: String = "NONE"
  }

  def base64Encode(in: String): Try[String] = Try {
    Base64.getEncoder.encodeToString(in.getBytes(StandardCharsets.UTF_8))
  }

  def base64Decode(in: String): Try[String] = Try {
    new String(Base64.getDecoder.decode(in.getBytes(StandardCharsets.UTF_8)))
  }

  val getWmlAuth: AuthType = {
    // We can force authorization type using config 'service.wml.auth'
    // so do not put a value in the fallback config files.
    Try {
      config.getString("service.wml.auth")
    } match {
      case Success(auth) =>
        auth.toUpperCase match {
          case "AUTH_IAM" => AUTH_IAM
          case "AUTH_ICP" => AUTH_ICP
          case _ =>
            if (isPublicCloud)
              AUTH_IAM
            else if (isPrivateCloud)
              AUTH_ICP
            else if (isDevelopment)
              AUTH_NONE
            else
              AUTH_IAM // default to IAM
        }
      case Failure(_) =>
        AUTH_IAM // default to IAM
    }
  }

  def getHost(host: String, port: Int): String = {
    val scheme: String = if (host.trim.toLowerCase.startsWith("http"))
      ""
    else
      "https://"
    val p: String = port match {
      case 0 | 80 | 443 => ""
      case n => s":$n"
    }
    s"$scheme$host$p"
  }

  private val usePrivateEndpoints = true

  private val platformHostImpl: Try[String] = {
    sys.env.get("PLATFORM_HOST") match {
      case Some(host) =>
        logger.info(s"Platform host: using environment variable PLATFORM_HOST $host")
        Success(host)
      case None =>
        Try(config.getString("service.platform.private.host")) match {
          case Success(host) if usePrivateEndpoints && !host.trim.isEmpty =>
            logger.info(s"""Platform host: using config "service.platform.private.host" $host""")
            Success(host)
          case _ =>
            Try(config.getString("service.platform.host")) match {
              case Success(host) if !host.trim.isEmpty =>
                logger.info(s"""Platform host: using config "service.platform.host" $host""")
                Success(host)
              case Success(host) =>
                Failure(ServiceException(StatusCodes.InternalServerError, InvalidConfigurationMessage(s"Found empty host '$host'")))
              case Failure(exception) =>
                Failure(exception)
            }
        }
    }
  }

  private val platformPortImpl: Int = {
    val DEFAULT_PORT = 0
    sys.env.get("PLATFORM_PORT") match {
      case Some(port) if Try(port.toInt).isSuccess =>
        logger.info(s"Platform port: using environment variable PLATFORM_PORT ${port.toInt}")
        port.toInt
      case _ =>
        Try(config.getInt("service.platform.private.port")) match {
          case Success(port) if usePrivateEndpoints =>
            logger.info(s"""Platform port: using config "service.platform.private.port" $port""")
            port
          case _ =>
            Try(config.getInt("service.platform.port")) match {
              case Success(port) =>
                logger.info(s"""Platform port: using config "service.platform.port" $port""")
                port
              case Failure(_) =>
                logger.info(s"Platform port: using default value $DEFAULT_PORT")
                DEFAULT_PORT
            }
        }
    }
  }

  def getPlatformURL: Try[URL] = Try {
    val host = platformHostImpl match {
      case Success(host) => host
      case Failure(exception) => throw exception
    }
    new URL(getHost(host, platformPortImpl))
  }

  def platformHost: Try[String] = {
    getPlatformURL match {
      case Success(url) => Success(url.getHost)
      case Failure(exception) => Failure(exception)
    }
  }

  def platformPort: Try[Int] = {
    getPlatformURL match {
      case Success(url) => Success(if (url.getPort > 0) url.getPort else url.getDefaultPort)
      case Failure(exception) => Failure(exception)
    }
  }

  val wmlPublicHost: Try[String] = {
    Try(config.getString("service.wml.host")) match {
      case Success(host) =>
        Success(host)
      case Failure(exception) =>
        Failure(exception)
    }
  }
  val wmlHost: Try[String] = {
    Try(config.getString("service.wml.private.host")) match {
      case Success(host) =>
        logger.info(s"""WML host using config "service.wml.private.host": $host""")
        Success(host)
      case Failure(exception) =>
        Failure(exception)
    }
  }
  val wmlPort: Int = {
    val DEFAULT_PORT = 443
    Try(config.getInt("service.wml.private.port")) match {
      case Success(port) =>
        logger.info(s"""WML port using config "service.wml.private.port": $port""")
        port
      case Failure(_) =>
        logger.info(s"wml port using default value $DEFAULT_PORT")
        DEFAULT_PORT
    }
  }

  val uploadContentMaxLength: Long = Try(config.getLong("service.upload-max-content-length")).getOrElse(1024 * 1024 * 1024 * 5) // 5GB

  // these are the HTTP headers for the request ID
  val HEADER_REQUEST_ID = "Request-ID"
  val HEADER_GLOBAL_TRANSACTION_ID = "x-global-transaction-id"

  // these are the headers used by the service (lazy because the config is defined in the 'service' module)
  lazy val IP_HEADERS: Vector[String] = getStringArray("service.ml-repository.ip-header-names")

  lazy val DOWNSTREAM_HEADERS: Vector[String] = getStringArray("service.ml-repository.downstream-header-names")
  lazy val DOWNSTREAM_EXCLUDE_HEADERS: Vector[String] = getStringArray("service.ml-repository.downstream-header-names-exclude")

  lazy val COPY_HEADERS: Vector[String] = getStringArray("service.ml-repository.copy-header-names")

  lazy val SECURITY_HEADERS: Vector[HttpHeader] = getStringMap("service.ml-repository.security-headers")

  lazy val NO_CACHE_HEADERS: Vector[HttpHeader] = getStringMap("service.ml-repository.no-cache-headers")

  private def getStringArray(name: String): Vector[String] = Try {
    config.getStringList(name)
  } match {
    case Success(headers) =>
      val h: ArrayBuffer[String] = ArrayBuffer.empty
      val it = headers.iterator()
      while (it.hasNext) {
        h += it.next()
      }
      h.toVector
    case _ =>
      throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(s"Not found: $name"))
  }

  private def getStringMap(name: String): Vector[HttpHeader] = Try {
    config.getStringList(name)
  } match {
    case Success(headers) =>
      val h: mutable.ArrayBuffer[HttpHeader] = mutable.ArrayBuffer.empty
      val it = headers.iterator()
      while (it.hasNext) {
        val name = it.next()
        h += RawHeader(name, it.next())
      }
      h.toVector
    case _ =>
      throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(s"Not found: $name"))
  }

  // these are the extra headers that as passed as-is to the underlying V4 services
  def downstreamServiceHeaders(request: HttpRequest): Map[String, String] = {
    def excludeFromDownstream(header: HttpHeader): Boolean = {
      DOWNSTREAM_EXCLUDE_HEADERS.exists(excluded => excluded.equalsIgnoreCase(header.name()))
    }
    val downstreamHeaders: mutable.Map[String, String] = mutable.Map.empty
    for (header <- request.headers) {
      if (!excludeFromDownstream(header)) {
        for (sentHeader <- DOWNSTREAM_HEADERS) {
          if ((sentHeader.endsWith("*") && header.lowercaseName().startsWith(sentHeader.substring(0, sentHeader.length - 1).toLowerCase))
            || header.name().equalsIgnoreCase(sentHeader)) {
            downstreamHeaders += (header.name -> header.value)
          }
        }
      }
    }
    downstreamHeaders.toMap
  }

  private val logPrettyPrint: Boolean = Try {
    config.getBoolean("service.ml-repository.log.pretty-print")
  } match {
    case Success(pp) => pp
    case Failure(_) =>
      true
  }

  private val logMasked: Boolean = Try {
    config.getBoolean("service.ml-repository.log.masked")
  } match {
    case Success(pp) => pp
    case Failure(_) => true
  }

  private val logTruncate: Option[Int] = Try {
    config.getInt("service.ml-repository.log.truncate")
  } match {
    case Success(tr) => Some(tr)
    case Failure(_) => None
  }

  // this can be used to explicitly mask a token
  def maskToken(token: String): String = {
    val t = token.substring(Math.min(token.length, 4))
    s"****${t.substring(Math.max(0, t.length - 4), t.length)}"
  }

  // any extra fields that we want to mask
  private val additionalFieldsToMask: Vector[String] = Vector("token", "rawToken", "raw_token")

  // pass by name so that we catch any exception during the conversion to json (if any) in the parameter
  def logPrint(js: => JsValue, masked: Option[Boolean] = None, prettyPrint: Option[Boolean] = None): String = {
    object MaskAndFormat extends JsonValuesMasking {
      private val m = masked match {
        case Some(md) => md
        case None => logMasked
      }
      private val p = prettyPrint match {
        case Some(pp) => pp
        case None => logPrettyPrint
      }

      def logPrint(js: JsValue): String = {
        (p, m) match {
          case (true, true) => js.masked(additionalFieldsToMask).prettyPrint
          case (true, false) => js.prettyPrint
          case (false, true) => js.masked(additionalFieldsToMask).compactPrint
          case (false, false) => js.compactPrint
        }
      }
    }
    // this must not throw an exception
    Try {
      MaskAndFormat.logPrint(js)
    } match {
      case Success(lp) =>
        logTruncate match {
          case Some(len) =>
            lp.substring(0, Math.min(len, lp.length))
          case None =>
            lp
        }
      case Failure(exception) =>
        ServiceException.getExceptionMessage(exception)
    }
  }

  // pass by name so that we catch any exception during the conversion to json (if any) in the parameter
  def maskedPrint(js: => JsValue, prettyPrint: Option[Boolean] = None): String = {
    logPrint(js, masked = Some(true), prettyPrint = prettyPrint)
  }

  def logHTTPHeaders(headers: Map[String, String]): String = {
    val sb = new StringBuilder()
    for (name <- headers.keySet) {
      if (sb.nonEmpty)
        sb.append(',')
      sb.append(name)
      sb.append(" -> ")
      val value = headers.getOrElse(name, "")
      if (Authorization.name.equalsIgnoreCase(name)) {
        sb.append(value.substring(0, Math.min(value.length, 20)))
      } else {
        sb.append(value)
      }
    }
    sb.toString()
  }

  private val UTC: DateFormat = {
    val df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss'Z'")
    df.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"))
    df
  }

  def formatAsDate(msecs: Long): String = {
    UTC.format(new Date(msecs))
  }

  private[utils] def formatAsDateNew(msecs: Long): String = {
    // this does not keep the milliseconds
    Try(DateTimeFormatter.ISO_INSTANT.format(Instant.ofEpochMilli(msecs))) match {
      case Success(value) => value
      case Failure(_) => UTC.format(new Date(msecs))
    }
  }

  def formatAsDuration(msecs: Long): String = {
    Try(Duration.ofMillis(msecs).toString) match {
      case Success(value) => value
      case Failure(_) => s"$msecs ms"
    }
  }

  def getDurationInSeconds(msecs: Long): String = {
    Try(Duration.ofMillis(msecs)) match {
      case Success(value) => s"${value.getSeconds}s"
      case Failure(_) => s"$msecs ms"
    }
  }

  def getDuration(name: String): FiniteDuration = {
    Try {
      config.getDuration(name, TimeUnit.MILLISECONDS)
    } match {
      case Success(dur) =>
        FiniteDuration(dur, TimeUnit.MILLISECONDS)
      case Failure(exception) =>
        throw exception
    }
  }

  def getDuration(name: String, fallback: FiniteDuration): FiniteDuration = {
    Try {
      getDuration(name)
    } match {
      case Success(dur) =>
        dur
      case Failure(_) =>
        fallback
    }
  }

  def getRequestId(request: HttpMessage): Option[String] = {
    request.headers.find(_.name().equalsIgnoreCase(HEADER_REQUEST_ID)) match {
      case Some(header) => Some(header.value())
      case _ => request.headers.find(_.name().equalsIgnoreCase(HEADER_GLOBAL_TRANSACTION_ID)) match {
        case Some(header) => Some(header.value())
        case _ => None
      }
    }
  }

  object V4RepositoryVersions {
    private[utils] def VD(date: String): VersionDate = {
      VersionDate(date) match {
        case Success(vd) =>
          vd
        case Failure(exception) =>
          // should never get here
          throw ServiceException(StatusCodes.InternalServerError, InternalErrorExceptionMessage(s"Failed to parse version date $date : ${ServiceException.getExceptionMessage(exception)}"))
      }
    }

    val V4_REPOSITORY_GA: VersionDate = VD(date = "2020-09-01")
  }

  def getContainer(spaceId: Option[String], projectId: Option[String]): Container =
    getContainer(spaceId, projectId, spaceOnly = false)

  def getContainer(spaceId: Option[String],
                   projectId: Option[String],
                   spaceOnly: Boolean,
                   isPost: Boolean = false): Container = {
    spaceId match {
      case Some(id) =>
        if (isValidUUID(id)) {
          Space(id)
        } else {
          val msg = "space_id must be a valid UUID"
          if(isPost) {
            throw ServiceException(StatusCodes.NotFound, InvalidRequestEntityMessage(msg))
          } else {
            throw ServiceException(StatusCodes.NotFound, InvalidQueryParameterMessage("space_id", id, Some(msg)))
          }
        }

      case None =>
        if (spaceOnly) {
          if (isPost) {
            val msg = "space_id has to be provided."
            throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
          } else
            throw ServiceException(StatusCodes.BadRequest, MissingQueryParameterMessage("space_id"))
        } else {
          projectId match {
            case Some(id) =>
              if (isValidUUID(id)) {
                Project(id)
              } else {
                val msg = "project_id must be a valid UUID"
                if(isPost) {
                  throw ServiceException(StatusCodes.NotFound, InvalidRequestEntityMessage(msg))
                } else {
                  throw ServiceException(StatusCodes.NotFound, InvalidQueryParameterMessage("project_id", id, Some(msg)))
                }
              }

            case None =>
              if (isPost) {
                val msg = "Either space_id or project_id has to be provided."
                throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
              } else
                throw ServiceException(StatusCodes.BadRequest, MissingOneOfQueryParametersMessage("space_id", "project_id"))
          }
        }
    }
  }

  def isValidUUID(id: String): Boolean = {
    Try(UUID.fromString(id)).filter(_.toString.equals(id)).isSuccess
  }

  // for the t to future function we need to make sure it never failed
  def seqFutures[T, U](inputs: Vector[T])(f: T => Future[U])
                      (implicit ec: ExecutionContext): Future[Vector[U]] = {
    val resBase = Future.successful(Vector.empty[U])
    inputs.iterator.foldLeft(resBase) { (futureRes, x) =>
      futureRes.flatMap {
        res => f(x).map(res :+ _)
      }
    }
  }

  /**
   * This method is used to know if this user id should be masked
   * from customers (because it is a WML allow-listed service id etc).
   *
   * @param userId The userId to test
   * @return <code>true</code> if this userId should be masked (so not seen by a customer).
   */
  def shouldMaskUserId(userId: String): Boolean = {
    if (isPublicCloud) {
      Try(StableServiceId.isStableServiceId(userId)) match {
        case Success(is) => is
        case Failure(_) =>
          Try(IAMStableProjectsServiceId.getAllowedList.contains(userId)).getOrElse(false)
      }
    } else if (isPrivateCloud) {
      false // for now we don't mask any id's in ICP
    } else {
      // development so no need to mask
      false
    }
  }

  // put all these under something specific to ICP?
  // Frank: sounds good

  val trainingResultRootPath: String = Try(config.getString("service.volumes.wml-training-data.path")).getOrElse("/wml-training-data")
}
