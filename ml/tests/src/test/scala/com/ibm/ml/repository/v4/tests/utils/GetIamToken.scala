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

import com.typesafe.config.{Config, ConfigFactory}
import org.apache.http.client.entity.UrlEncodedFormEntity
import org.apache.http.client.methods.{CloseableHttpResponse, HttpPost}
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.message.BasicNameValuePair
import org.apache.http.{Consts, HttpHeaders}
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.Assertions
import spray.json._
import FormatUtils._

import scala.util.{Failure, Success, Try}

case class GetIamToken(httpClient: CloseableHttpClient) extends IAMTokenGenerator with Assertions {

  // these are set in the pom or the reference.conf and normally don't need to be overridden
  val CONFIG_CACHE_IAM_TOKEN: String = "fvt.iam.cache-token"
  val CONFIG_IAM_TOKEN_SERVER: String = "fvt.iam.token_server"
  val CONFIG_IAM_AUTHORIZATION: String = "fvt.iam.authorization"

  private val logger: Logger = LogManager.getLogger(GetIamToken.getClass)

  private lazy val config: Config = ConfigFactory.load()

  private val cacheIamToken: Boolean = {
    Try(config.getBoolean(CONFIG_CACHE_IAM_TOKEN)).getOrElse(true) // by default we cache the IAM token
  }

  private val iamAuthenticationHeaders: Map[String, Map[String, String]] = Map()

  private def getIamToken(apiKey: String): Try[String] = Try {
    // https://console.bluemix.net/apidocs/iam-identity-token-api#create-an-iam-access-token-for-a-user-or-service-i
    val post: HttpPost = new HttpPost(stripQuotes(config.getString(CONFIG_IAM_TOKEN_SERVER)))
    setHeaders(post,
      Map(
        HttpHeaders.AUTHORIZATION -> stripQuotes(config.getString(CONFIG_IAM_AUTHORIZATION)),
        HttpHeaders.CONTENT_TYPE -> "application/x-www-form-urlencoded",
        HttpHeaders.ACCEPT -> "application/json",
        V4TestServicesClient.HEADER_GLOBAL_TRANSACTION_ID -> "get-iam-token"
      )
    )
    val params = Vector(
      new BasicNameValuePair("grant_type", "urn:ibm:params:oauth:grant-type:apikey"),
      new BasicNameValuePair("apikey", apiKey)
    )
    import scala.jdk.CollectionConverters._
    post.setEntity(new UrlEncodedFormEntity(params.asJava, Consts.UTF_8))
    var response: CloseableHttpResponse = null
    try {
      response = execute(request = post, doLog = false)
      val entity = Try {
        getEntity(response)
      } match {
        case Success(ent) =>
          ent
        case Failure(exception) =>
          logger.warn(s"Got exception when trying to get IAM response entity: ${exception.getMessage}")
          None
      }
      if (response.getStatusLine.getStatusCode == 200) entity match {
        case Some(json) =>
          case class IamResponse(accessToken: String, refreshToken: String, tokenType: String)
          object IamResponseFormat extends DefaultJsonProtocol {
            implicit lazy val iamResponseFormat: RootJsonFormat[IamResponse] = jsonFormat(IamResponse,
              "access_token",
              "refresh_token",
              "token_type")
          }
          import IamResponseFormat._
          val iamResponse = JsonParser(json).convertTo[IamResponse]
          logger.info(s"Got IAM token (accessToken): ${iamResponse.toJson.prettyPrint}")
          s"${iamResponse.tokenType} ${iamResponse.accessToken}"
        case None =>
          fail(s"Failed to get json reply: ${serialize(response)}")
      } else
        fail(s"Failed to get IAM token: ${serialize(response)} $entity")
    }
    finally {
      close(response)
    }
  }

  def getAuthenticationHeaders(apiKey: String): Map[String, String] = {
    Try {
      iamAuthenticationHeaders.get(apiKey).get
    } match {
      case Success(headers: Map[String, String]) => return headers
      case _ => // continue
    }

    val bearerToken: String = getIamToken(apiKey) match {
      case Success(token) =>
        if (token.startsWith("Bearer ")) token else s"Bearer $token"
      case Failure(ex) =>
        fail(s"IAM token generation failure: ${ex.getMessage}")
    }
    logger.info(s"Using authorization $bearerToken")
    val headers = Map(
      HttpHeaders.AUTHORIZATION -> bearerToken
    )
    if (cacheIamToken) {
      iamAuthenticationHeaders ++ Map(apiKey -> headers)
    }
    headers
  }
}
