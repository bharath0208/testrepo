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

import java.io.FileInputStream
import java.net.InetAddress
import java.security.{KeyStore, SecureRandom}

import akka.http.scaladsl.model.{HttpRequest, HttpResponse, StatusCodes}
import akka.http.scaladsl.server.Rejection
import com.ibm.analytics.wml.utils.security.ConfigDecrypter
import com.ibm.ml.repository.v4.utils.errors.ServiceRejectionHandler
import com.ibm.ml.repository.v4.utils.logging.{AccessLogger, reqId}
import com.typesafe.scalalogging.StrictLogging
import javax.net.ssl.{KeyManagerFactory, SSLContext, TrustManagerFactory}

import scala.util.{Failure, Success, Try}

trait ServiceUtils extends ConfigDecrypter with StrictLogging {
  protected def getLocalHostName: String = {
    Try {
      val host = InetAddress.getLocalHost
      val ips = InetAddress.getAllByName(host.getHostName)
      // always use wml if we find it
      for (ip <- ips) {
        if (ip.getHostName.startsWith("wml"))
          return ip.getHostName
      }
      for (ip <- ips) {
        if (ip.getHostAddress.startsWith("192."))
          return ip.getHostAddress
      }
      host.getHostAddress
    } match {
      case Success(ip) => ip
      case Failure(_) => "unknown"
    }
  }

  protected def useSsl: Boolean = {
    Try {
      config.getConfig("service.ssl.keystore")
    } match {
      case Success(_) =>
        true
      case Failure(_) =>
        throw ServiceException(
          StatusCodes.InternalServerError,
          SSLConfigFailedMessage()
        )
    }
  }

  protected def logRequest(request: HttpRequest): Unit = {
    val requestId: Option[String] = getRequestId(request)
    reqId(requestId)(() => logger.debug(s"Request ${request.method.value} ${request.uri}"))
  }

  protected def logResponse(request: HttpRequest, requestStarted: Long)(response: HttpResponse): HttpResponse = {
    val now: Long = System.currentTimeMillis()
    val requestId: Option[String] = getRequestId(request)
    reqId(requestId)(() => logger.debug(s"Response ${request.method.value} ${request.uri} ${response.status.intValue()} ${now - requestStarted}"))
    AccessLogger.logAccess(request, None, response.status.intValue(), requestStarted, now)
    response
  }

  protected def logRejections(request: HttpRequest, requestStarted: Long)(rejections: Seq[Rejection]): Seq[Rejection] = {
    rejections match {
      case Nil =>
        return rejections
      case _ =>
    }

    def getRejectionReason(rejections: Seq[Rejection]): (Int, String) = {
      val rejection = rejections.head
      (ServiceRejectionHandler.getStatus(rejection).intValue(), rejection.getClass.getSimpleName)
    }

    val now: Long = System.currentTimeMillis()
    val requestId: Option[String] = getRequestId(request)
    val reason = getRejectionReason(rejections)
    reqId(requestId)(() => logger.debug(s"Response ${request.method.value} ${request.uri} ${reason._1} ${now - requestStarted}"))
    AccessLogger.logAccess(request, None, reason._1, requestStarted, now)
    rejections
  }

  protected def checkConfigString(description: String, value: String): String = {
    if ((value != null) && value.contains("${")) {
      logger.error(s"Failed to expand $description : $value")
      throw ServiceException(
        StatusCodes.InternalServerError,
        SSLConfigFailedMessage()
      )
    }
    value
  }

  protected def decode(encoded: String): Array[Char] = {
    Try {
      config.getBoolean("service.ssl.keystore.decrypt")
    } match {
      case Success(doit) =>
        if (doit)
          decrypt(encoded).toCharArray
        else
          encoded.toCharArray
      case Failure(_) =>
        //decrypt(encoded).toCharArray
        encoded.toCharArray
    }
  }

  protected def getSSLContext(sc: ContextUtils): SSLContext = {
    val ksFile = sc.getAsFile(config, "service.ssl.keystore.path")
    if (!ksFile.exists) {
      logger.error(s"Failed to find the keystore ${ksFile.getAbsolutePath}")
      throw ServiceException(
        StatusCodes.InternalServerError,
        SSLConfigFailedMessage()
      )
    }

    val password: Array[Char] = decode(checkConfigString("keystore password", config.getString("service.ssl.keystore.password")))
    val ksType: String = checkConfigString("keystore.type", config.getString("service.ssl.keystore.type"))

    val ks: KeyStore = KeyStore.getInstance(ksType)
    val keystore = new FileInputStream(ksFile)
    try {
      Try {
        require(keystore != null, s"Null keystore $ksFile could not be loaded!")
        ks.load(keystore, password)
      } match {
        case Success(_) => // ok
        case Failure(exception) =>
          logger.error(s"Failed to load the keystore ${ksFile.getAbsolutePath} with password ${new String(password)}: ${ServiceException.getExceptionMessage(exception)}")
          throw ServiceException(
            StatusCodes.InternalServerError,
            SSLConfigFailedMessage()
          )
      }
    }
    finally {
      Try {
        keystore.close()
      }
    }

    val keyManagerFactory = KeyManagerFactory.getInstance(Try(config.getString("service.ssl.key-manager-factory")).getOrElse("SunX509"))
    keyManagerFactory.init(ks, password)

    val trustManagerFactory: TrustManagerFactory = TrustManagerFactory.getInstance(Try(config.getString("service.ssl.trust-manager-factory")).getOrElse("SunX509"))
    trustManagerFactory.init(ks)

    val context = SSLContext.getInstance(Try(config.getString("service.ssl.context")).getOrElse("TLS"))
    context.init(keyManagerFactory.getKeyManagers, trustManagerFactory.getTrustManagers, new SecureRandom)
    context
  }
}
