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

case class GetIamUAATokens(httpClient: CloseableHttpClient) extends IAMTokenGenerator with Assertions {
  // these are set in the pom or the reference.conf and normally don't need to be overridden
  val CONFIG_CACHE_IAM_TOKEN: String = "fvt.iam.cache-token"
  val CONFIG_IAM_TOKEN_SERVER: String = "fvt.iam.token_server"
  val CONFIG_IAM_AUTHORIZATION: String = "fvt.iam.authorization"
  val CONFIG_IAM_USERNAME: String = "fvt.iam.username"
  val CONFIG_IAM_PASSWORD: String = "fvt.iam.password"
  val CONFIG_IAM_UAA_CLIENT_ID: String = "fvt.iam.uaa_client_id"
  val CONFIG_IAM_UAA_CLIENT_SECRET: String = "fvt.iam.uaa_client_secret"
  val CONFIG_IAM_BSS_ACCOUNT_ID: String = "fvt.iam.bss_account_id"

  private val X_UAA_Authorization = "X-UAA-Authorization"

  private val logger: Logger = LogManager.getLogger(GetIamToken.getClass)

  private lazy val config: Config = ConfigFactory.load()

  private val cacheIamToken: Boolean = {
    Try(config.getBoolean(CONFIG_CACHE_IAM_TOKEN)).getOrElse(true) // by default we cache the IAM token
  }

  private val iamAuthenticationHeaders: Map[String, Map[String, String]] = Map()

  private def getIamToken(apiKey: String): Try[(String, String)] = Try {
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
      new BasicNameValuePair("grant_type", "password"),
      //new BasicNameValuePair("apikey", apiKey),
      new BasicNameValuePair("response_type", "cloud_iam,uaa"),
      new BasicNameValuePair("username", stripQuotes(config.getString(CONFIG_IAM_USERNAME))),
      new BasicNameValuePair("password", stripQuotes(config.getString(CONFIG_IAM_PASSWORD))),
      new BasicNameValuePair("uaa_client_id", stripQuotes(config.getString(CONFIG_IAM_UAA_CLIENT_ID))),
      new BasicNameValuePair("uaa_client_secret", stripQuotes(config.getString(CONFIG_IAM_UAA_CLIENT_SECRET))),
      new BasicNameValuePair("bss_account", stripQuotes(config.getString(CONFIG_IAM_BSS_ACCOUNT_ID))),
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
          case class IamUAAResponse(accessToken: String, uaaToken: String, uaaRefreshToken: String, refreshToken: String, tokenType: String)
          object IamUAAResponseFormat extends DefaultJsonProtocol {
            implicit lazy val iamResponseFormat: RootJsonFormat[IamUAAResponse] = jsonFormat(IamUAAResponse,
              "access_token",
              "uaa_token",
              "uaa_refresh_token",
              "refresh_token",
              "token_type")
          }
          import IamUAAResponseFormat._
          val iamUaaResponse = JsonParser(json).convertTo[IamUAAResponse]
          logger.info(s"Got IAM token (accessToken): ${iamUaaResponse.toJson.prettyPrint}")
          (s"${iamUaaResponse.tokenType} ${iamUaaResponse.accessToken}", s"${iamUaaResponse.tokenType} ${iamUaaResponse.uaaToken}")
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

    val tokens: (String, String) = getIamToken(apiKey) match {
      case Success((accessToken, uaaToken)) => {
        val accessTokenFinal = if (accessToken.startsWith("Bearer ")) accessToken else s"Bearer $accessToken"
        val uaaTokenFinal = if (uaaToken.startsWith("Bearer ")) uaaToken else s"Bearer $uaaToken"
        (accessTokenFinal, uaaTokenFinal)
      }
      case Failure(ex) =>
        fail(s"IAM token generation failure: ${ex.getMessage}")
    }
    logger.info(s"Using authorization ${tokens._1}, UAA ${tokens._2}")
    val headers = Map(
      HttpHeaders.AUTHORIZATION -> tokens._1,
      X_UAA_Authorization -> tokens._2
    )
    if (cacheIamToken)
      iamAuthenticationHeaders ++ Map(apiKey -> headers)
    headers
  }
}
