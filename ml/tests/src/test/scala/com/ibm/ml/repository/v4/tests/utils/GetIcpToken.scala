/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.utils

import java.nio.charset.StandardCharsets
import java.util.Base64

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.http.HttpHeaders
import org.apache.http.client.methods.HttpGet
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.util.EntityUtils
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.Assertions
import spray.json.JsonParser
import FormatUtils._

import scala.util.{Failure, Success, Try}

case class GetIcpToken(httpClient: CloseableHttpClient) extends HttpTestClient with Assertions {
  private val logger: Logger = LogManager.getLogger(GetIcpToken.getClass)

  private lazy val config: Config = ConfigFactory.load()

  private def getIcpToken(userName: String = stripQuotes(config.getString("icp.token.username")),
                          password: String = stripQuotes(config.getString("icp.token.password")),
                          tokenEndpoint: String = stripQuotes(config.getString("icp.token.url"))): Try[String] = {
    val authString = userName + ":" + password
    val encodedAuth = Base64.getEncoder.encodeToString(authString.getBytes(StandardCharsets.UTF_8))
    val getReq = new HttpGet(s"$tokenEndpoint/v1/preauth/validateAuth")
    getReq.addHeader("Authorization", "Basic " + encodedAuth)
    val response = httpClient.execute(getReq)
    val statusCode = response.getStatusLine.getStatusCode
    logger.info(s"Create icpToken on $tokenEndpoint/v1/preauth/validateAuth with status $statusCode")
    try {
      if (statusCode == 200) {
        val responseJson = JsonParser(EntityUtils.toString(response.getEntity))
        logger.info(s"response is $responseJson")
        import spray.json._
        val token = responseJson.toString().parseJson.asJsObject.fields("accessToken").toString().replace("\"", "")
        logger.info(s"Got ICP token $token")
        Success(token)
      } else {
        val rawResponse = EntityUtils.toString(response.getEntity)
        Failure(new Exception(s"Unable to obtain an ICP Token for authentication [$statusCode] - $rawResponse"))
      }
    } finally {
      httpClient.close()
    }
  }

  def getBasicAuthenticationHeaders: Map[String, String] = {
    val authToken = stripQuotes(config.getString("service.token"))
    val authString = if( authToken.startsWith("Basic") || authToken.startsWith("basic") ) authToken else s"Basic $authToken"
    Map(HttpHeaders.AUTHORIZATION -> authString)
  }

  def getAuthenticationHeaders: Map[String, String] = {
    val bearerToken: String = getIcpToken() match {
      case Success(token) =>
        if (token.startsWith("Bearer ")) token else s"Bearer $token"
      case Failure(ex) =>
        fail(s"ICP token generation failure: ${ex.getMessage}")
    }
    logger.info(s"Using authorization $bearerToken")
    Map(
      HttpHeaders.AUTHORIZATION -> bearerToken,
      "ML-Instance-ID" -> "wml_local"
    )
  }
}
