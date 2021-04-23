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

import java.net.InetAddress

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri.Host
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, OAuth2BearerToken}
import akka.http.scaladsl.model.{ContentTypes, Uri}
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.ibm.analytics.wml.service.utils.security.AuthorizationException
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableServiceId}
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.analytics.wml.utils.clients.http.{AkkaHttpClient, BasicAuthProvider, HttpClientBuilder, RetryPolicy}
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging._
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import spray.json.{JsString, JsonParser}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object CAMSAuthenticator extends StrictLogging {
  private def createHttpClient(clientLoader: HttpClientBuilder,
                               url: Uri,
                               requestId: Option[String]): Future[AkkaHttpClient] = {
    def host: String = {
      def getHostName(addresses: Seq[InetAddress]): String = {
        // try to get by name ignoring local host
        for (address <- addresses) {
          if (!address.isAnyLocalAddress && (address.getHostName != null))
            return address.getHostName
        }
        // try to get by address ignoring local host
        for (address <- addresses) {
          if (!address.isAnyLocalAddress && (address.getHostAddress != null))
            return address.getHostAddress
        }
        // try to get by name
        for (address <- addresses) {
          if (address.getHostName != null)
            return address.getHostName
        }
        // try to get by address
        for (address <- addresses) {
          if (address.getHostAddress != null)
            return address.getHostAddress
        }
        // should never get here
        throw CommandLineArgsException(s"The service URL '$url' does not contain a valid host name")
      }

      if (url.authority.isEmpty)
        throw CommandLineArgsException(s"The service URL '$url' does not contain an authority")
      val host: Host = url.authority.host
      if (host.isEmpty())
        throw CommandLineArgsException(s"The service URL '$url' does not contain a host")
      if ((host.address == null) || host.address().trim.isEmpty)
        getHostName(host.inetAddresses)
      else
        host.address()
    }

    val hostname: String = host
    val port: Int = url.effectivePort
    reqId(requestId)(() => logger.info(s"Creating AkkaHttpClient for host $hostname and port $port"))
    Future.fromTry {
      Try {
        clientLoader.get(hostname, port, HttpClientBuilder.Keys.CAMS_CLIENT)
      } match {
        case Success(client) =>
          Success(client)
        case Failure(exception) =>
          exception match {
            case e: GlobalAssetTypesException =>
              Failure(e)
            case e: Throwable =>
              Failure(AuthenticationException(s"Failed to create new HTTP client", Some(e)))
          }
      }
    }
  }

  private def getIAMAuthentication(commands: CommandLine,
                                   clientLoader: HttpClientBuilder,
                                   retryPolicy: RetryPolicy)
                                  (implicit system: ActorSystem): Future[Map[String, String]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    def getIAMTokenUrl: Uri = {
      if (commands.isProductionPublicCloud)
        "https://iam.cloud.ibm.com/oidc/token"
      else
        "https://iam.test.cloud.ibm.com/oidc/token"
    }

    implicit val credentialsProvider: Option[HttpCredentialsProvider] = Some(BasicAuthProvider("Yng6Yng="))
    implicit val retries: RetryPolicy = retryPolicy

    val tokenURL = getIAMTokenUrl
    val apiKey = commands.auth

    for {
      _ <- Future(reqId(commands.requestId)(() => logger.info(s"IAM authentication URL: $tokenURL for API Key: $apiKey")))
      client <- createHttpClient(clientLoader, tokenURL, commands.requestId)
      response <- client.post(
        uri = tokenURL.path.toString(),
        body = Some(
          ContentTypes.`application/x-www-form-urlencoded`,
          s"grant_type=urn:ibm:params:oauth:grant-type:apikey&apikey=$apiKey"
        )
      )
      json <- client.toJson(response).recoverWith{
        case f:Throwable => Try(response.discardEntityBytes()).recover { case t => logger.warn("Unable to discard HttpResponse content", t) }
          Future.failed(f)
      }
    } yield {
      json.asJsObject.fields.get("access_token") match {
        case Some(token: JsString) =>
          val credentials = OAuth2BearerToken(token.value)
          reqId(commands.requestId)(() => logger.info(s"Created IAM credentials: ${credentials.scheme()} ${credentials.token}"))
          Map(Authorization.name -> s"${credentials.scheme()} ${credentials.token}")
        case Some(token) =>
          throw AuthenticationException(s"Found 'access_token' in IAM response but not of right type (${token.getClass.getName}): ${logPrint(json)}")
        case None =>
          throw AuthenticationException(s"Failed to find 'access_token' in IAM response: ${logPrint(json)}")
      }
    }
  }

  private def getICPAuth(config: Config,
                         builder: HttpClientBuilder,
                         requestId: Option[String])
                        (implicit system: ActorSystem,
                         retryPolicy: RetryPolicy): Future[String] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val url = Uri(config.getString("icp.token.url"))
    val validateUrl = url.withPath(Uri.Path("/v1/preauth/validateAuth"))
    val client = builder.get(url.authority.host.address(), if (url.authority.port > 0) url.authority.port else 443, HttpClientBuilder.Keys.CAMS_CLIENT)
    val credentialsProvider = HttpCredentialsProvider(BasicHttpCredentials(config.getString("icp.token.username"), config.getString("icp.token.password")))
    for {
      response <- client.get(
        uri = validateUrl.toString(),
        action = Some("get-icp-token")
      )(Some(credentialsProvider), retryPolicy)
      json <- Unmarshal(response.entity).to[String].recoverWith {
        case t =>
          Try(response.discardEntityBytes()).recover {
            case t =>
              logger.warn("Unable to discard HttpResponse content", t)
          }
          Future.failed(t)
      }
      token <- Future.fromTry {
        Try {
          val js = JsonParser(json).asJsObject
          js.fields.get("accessToken") match {
            case Some(token) =>
              token match {
                case JsString(token) =>
                  token
                case _ =>
                  throw AuthenticationException(s"Wrong type for 'accessToken' in $json")
              }
            case None =>
              throw AuthenticationException(s"Failed to to find 'accessToken' in $json")
          }
        }
      }
    } yield {
      reqId(requestId)(() => logger.info(s"Using ICP token: $token"))
      token
    }
  }

  private def getICPAuthentication(commands: CommandLine,
                                   clientLoader: HttpClientBuilder,
                                   retryPolicy: RetryPolicy)
                                  (implicit system: ActorSystem): Future[Map[String, String]] = {
    val credentials = BasicHttpCredentials(commands.auth)
    Future.successful(Map(Authorization.name -> s"${credentials.scheme()} ${credentials.cookie.mkString}"))
    // val credentials = OAuth2BearerToken(commands.auth)
    // Future.successful(Map(Authorization.name -> credentials.value))
  }

  def getAuthDetails(commands: CommandLine,
                     clientLoader: HttpClientBuilder,
                     retryPolicy: RetryPolicy)
                    (implicit system: ActorSystem,
                     iamContext: IAMContext): Future[Map[String, String]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    // if we provide a basic or bearer token use as-is
    if (commands.auth.startsWith("Basic ") || commands.auth.startsWith("Bearer ")) {
      Future.successful(Map(Authorization.name -> commands.auth))
    } else if (!commands.isICP && "service-id".equalsIgnoreCase(commands.auth)) {
      {
        for {
          token <- IAMStableServiceId()(iamContext).createServiceIdToken()(system, clientLoader)
        } yield {
          val ob = OAuth2BearerToken(token)
          logger.info("Using WML service id token for CAMS global asset type registration")
          Map(
            Authorization.name -> ob.value
          )
        }
      } recoverWith {
        case ae: AuthorizationException =>
          throw AuthenticationException("Failed to get the service id for authentication", Some(ae))
        case e: Throwable =>
          throw AuthenticationException("Failed to get the service id for authentication", Some(e))
      }
    } else {
      {
        for {
          auth <- {
            if (commands.isICP)
              getICPAuthentication(commands, clientLoader, retryPolicy)
            else
              getIAMAuthentication(commands, clientLoader, retryPolicy)
          }
        } yield {
          auth
        }
      } recoverWith {
        case e: GlobalAssetTypesException =>
          throw e
        case e: Throwable =>
          throw AuthenticationException("Failed to get the authentication details", Some(e))
      }
    }
  }
}
