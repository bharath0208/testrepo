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

import java.io.Closeable
import java.security.SecureRandom
import java.security.cert.X509Certificate

import com.ibm.ml.repository.v4.utils._
import com.typesafe.scalalogging.StrictLogging
import javax.net.ssl.X509TrustManager
import org.apache.http.client.config.RequestConfig
import org.apache.http.client.methods.{CloseableHttpResponse, HttpGet, HttpPost, HttpRequestBase}
import org.apache.http.conn.ssl.NoopHostnameVerifier
import org.apache.http.entity.{ContentType, StringEntity}
import org.apache.http.impl.client.{CloseableHttpClient, HttpClientBuilder}
import org.apache.http.ssl.SSLContexts
import org.apache.http.util.EntityUtils
import org.apache.http.{HttpEntity, HttpException, HttpHeaders, HttpResponse}
import spray.json.{JsValue, JsonParser}

import scala.math.min
import scala.util.{Failure, Success, Try}

object TestClient extends StrictLogging {
  private val TEST_ENTITY_HEADER = "__TEST_HACK__"
  private val TOKEN_PRINT_LENGTH = 40

  def getEntity(response: HttpResponse): Option[String] = {
    // this is a hack so that we can call this method more than once
    val TEST_ENTITY_EMPTY = "__TEST_EMPTY__"
    require(response != null)
    if (response.getFirstHeader(TEST_ENTITY_HEADER) != null) {
      val ent = response.getFirstHeader(TEST_ENTITY_HEADER).getValue
      if (TEST_ENTITY_EMPTY.equals(ent))
        None
      else
        Some(ent)
    } else {
      val ent = {
        if (response.getEntity != null) {
          Try {
            EntityUtils.toString(response.getEntity)
          } match {
            case Success(resp) =>
              if ((resp != null) && !resp.trim.isEmpty)
                Some(resp)
              else
                None
            case Failure(_) =>
              // log?
              None
          }
        } else
          None
      }
      ent match {
        case Some(e) => response.addHeader(TEST_ENTITY_HEADER, e)
        case None => response.addHeader(TEST_ENTITY_HEADER, TEST_ENTITY_EMPTY)
      }
      ent
    }
  }

  def getStatus(response: HttpResponse): Int = {
    response.getStatusLine.getStatusCode
  }

  private def doLog(st: Int, bodyOp: Option[String]): Unit = {
    logger.info(
      s"""
         |------------ response ($st) --------------
         |${bodyOp.getOrElse("No content returned")}
         |------------------------------------------
       """.stripMargin
    )
  }

  def getAndLogResponse(response: HttpResponse): Option[JsValue] = {
    Try {
      getEntity(response)
    } match {
      case Success(entity) =>
        entity match {
          case Some(ent) =>
            Try {
              JsonParser(ent)
            } match {
              case Success(js) =>
                doLog(getStatus(response), Some(js.prettyPrint))
                Some(js)
              case _ if ent != "" =>
                doLog(getStatus(response), Some(ent))
                None
              case _ =>
                doLog(getStatus(response), None)
                None
            }
          case None =>
            doLog(getStatus(response), None)
            None
        }
      case Failure(e) =>
        doLog(getStatus(response), Some(e.getMessage))
        None
    }
  }

  private def trim(value: String, len: Int): String = {
    if (value.length > len)
      value.substring(0, min(len, value.length)) + "..."
    else
      value
  }

  private def getHeader(request: HttpRequestBase,
                        name: String,
                        padding: Option[String],
                        len: Option[Int]): String = {
    val s: StringBuilder = new StringBuilder()
    if (request.getFirstHeader(name) != null) {
      padding match {
        case Some(pad) => s.append(pad)
        case None =>
      }
      val h = request.getFirstHeader(name).toString
      len match {
        case Some(length) => s.append(trim(h, length))
        case None => s.append(h)
      }
    }
    s.toString
  }

  def serialize(request: HttpRequestBase): String = {
    val s: StringBuilder = new StringBuilder()
    s.append(request.getMethod)
    s.append(' ')
    s.append(request.getURI)
    s.append(getHeader(request, HttpHeaders.AUTHORIZATION, Some(" "), Some(TOKEN_PRINT_LENGTH)))
    s.append(getHeader(request, HEADER_REQUEST_ID, Some(" "), None))
    s.toString
  }

  def serialize(response: HttpResponse): String = {
    val s: StringBuilder = new StringBuilder()
    s.append(response.getStatusLine.getStatusCode)
    for (header <- response.getAllHeaders) {
      if (TEST_ENTITY_HEADER.equalsIgnoreCase(header.getName)) {
        // ignore
      } else {
        s.append(' ')
        s.append(header.getName)
        s.append('=')
        if (HttpHeaders.AUTHORIZATION.equalsIgnoreCase(header.getName))
          s.append(trim(header.getValue, TOKEN_PRINT_LENGTH))
        else
          s.append(header.getValue)
      }
    }
    if (response.getFirstHeader(HttpHeaders.CONTENT_LENGTH) != null) {
      if (Integer.parseInt(response.getFirstHeader(HttpHeaders.CONTENT_LENGTH).getValue) > 0) {
        s.append(' ')
        s.append(getEntity(response))
      }
    }
    s.toString
  }

  def jsonEntity(entity: Option[String]): Option[HttpEntity] = {
    entity match {
      case Some(ec) => Some(new StringEntity(JsonParser(ec).prettyPrint, ContentType.APPLICATION_JSON))
      case None => None
    }
  }

  def getHttpsClient(noRetries: Boolean): CloseableHttpClient = {
    val timeout = 100000
    val maxConnections = 50
    val sslContext = SSLContexts.createDefault()
    sslContext.init(null, Array(new X509TrustManager {
      override def getAcceptedIssuers: Array[X509Certificate] = Array.empty[X509Certificate]

      override def checkClientTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}

      override def checkServerTrusted(x509Certificates: Array[X509Certificate], s: String): Unit = {}
    }), new SecureRandom())
    val requestConfig: RequestConfig = RequestConfig
      .custom()
      .setConnectTimeout(timeout)
      .setConnectionRequestTimeout(timeout)
      .setSocketTimeout(timeout)
      .build()
    val builder = HttpClientBuilder
      .create()
      // create a pooling client
      .setMaxConnTotal(maxConnections)
      .setMaxConnPerRoute(maxConnections)
      .setSSLHostnameVerifier(NoopHostnameVerifier.INSTANCE)
      .setSSLContext(sslContext)
      .setDefaultRequestConfig(requestConfig)
      .setConnectionManagerShared(true)
    if (noRetries) builder.disableAutomaticRetries()
    builder.build()
  }

  def apply(): TestClient = {
    new TestClient(getHttpsClient(false))
  }
}

import com.ibm.ml.repository.v4.migration.utils.TestClient._

case class TestClient(httpClient: CloseableHttpClient) extends Closeable with StrictLogging {

  override def close(): Unit = {
    close(httpClient)
  }

  // this must be used to close a response when done
  protected def close(closeable: Closeable): Unit = {
    if (closeable != null) {
      try {
        closeable.close()
      }
      catch {
        case _: Exception => // ignore
      }
    }
  }

  protected def logRequestId(request: HttpRequestBase): String = {
    if (request.getHeaders(HEADER_REQUEST_ID).length > 0) {
      s"[${request.getFirstHeader(HEADER_REQUEST_ID).getValue}]"
    } else
      ""
  }

  protected def logRequest(response: HttpResponse, request: HttpRequestBase, start: Long): Unit = {
    val duration = System.currentTimeMillis() - start
    val entity: String = request match {
      case post: HttpPost =>
        val en = post.getEntity
        if (en != null)
          en.toString
        else
          ""
      case _ => ""
    }
    logger.trace(s"Request: ${request.getURI} ${logRequestId(request)} $entity [$duration ms] response: ${serialize(response)}")
  }

  protected def singleRequest(request: HttpRequestBase): CloseableHttpResponse = {
    val start: Long = System.currentTimeMillis()
    val response = httpClient.execute(request)
    logRequest(response, request, start)
    response
  }

  protected def getHeaders(requestId: Option[String]): Map[String, String] = {
    if (requestId.nonEmpty) Map(HEADER_GLOBAL_TRANSACTION_ID -> requestId.get) else Map()
  }

  protected def isExpected(status: Int, expectedStatus: Iterable[Int]): Boolean = {
    if (expectedStatus.isEmpty)
      status < 400
    else {
      for (st <- expectedStatus)
        if (st == status)
          return true
      false
    }
  }

  protected def handleRequest(request: HttpRequestBase,
                              expectedStatus: Iterable[Int] = None,
                              responseCanBeMissing: Boolean = false): CloseableHttpResponse = {
    val response = singleRequest(request = request)
    try {
      val resp: Option[String] = getEntity(response)
      if (isExpected(response.getStatusLine.getStatusCode, expectedStatus))
        resp match {
          case Some(_) =>
            response
          case None =>
            if (responseCanBeMissing)
              response
            else
              throw new HttpException(s"Failed to get a json entity: ${serialize(response)}")
        }
      else
        throw new HttpException(s"Bad status code ${response.getStatusLine.getStatusCode} for ${serialize(request)} when expecting ${expectedStatus.mkString(",")}: ${if (resp.isDefined) s"\n${resp.get}" else ""}")
    }
    finally {
      close(response)
    }
  }

  def setHeaders(request: HttpRequestBase,
                 headers: Map[String, String]): Unit = {
    headers foreach {
      case (key, value) =>
        request.addHeader(key, value)
    }
  }

  def acceptJson(request: HttpRequestBase): Unit = {
    request.addHeader(HttpHeaders.ACCEPT, "application/json")
  }

  def execute(request: HttpRequestBase,
              requestId: Option[String] = None,
              expectedStatus: Iterable[Int] = None,
              authenticationHeaders: Map[String, String] = Map()): CloseableHttpResponse = {
    setHeaders(request, getHeaders(requestId) ++ authenticationHeaders)
    acceptJson(request)
    handleRequest(request = request, expectedStatus = expectedStatus)
  }

  def get(uri: String,
          requestId: Option[String],
          expectedStatus: Iterable[Int] = Some(200),
          authenticationHeaders: Map[String, String] = Map()): CloseableHttpResponse = {
    val get = new HttpGet(uri)
    setHeaders(get, getHeaders(requestId) ++ authenticationHeaders)
    acceptJson(get)
    handleRequest(request = get, expectedStatus = expectedStatus)
  }

  def post(uri: String,
           entity: Option[HttpEntity],
           requestId: Option[String],
           expectedStatus: Iterable[Int],
           authenticationHeaders: Map[String, String] = Map()): HttpResponse = {
    val post = new HttpPost(uri)
    setHeaders(post, getHeaders(requestId) ++ authenticationHeaders)
    acceptJson(post)
    entity match {
      case Some(en) =>
        post.setEntity(en)
      case None =>
        None
    }
    handleRequest(request = post, expectedStatus = expectedStatus)
  }
}
