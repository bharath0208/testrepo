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

import java.util.Base64
import java.util.concurrent.TimeoutException

import akka.actor.ActorSystem
import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.headers.BasicHttpCredentials
import akka.http.scaladsl.unmarshalling.Unmarshal
import com.ibm.analytics.wml.service.utils.security.StableServiceId
import com.ibm.analytics.wml.service.utils.security.iam.IAMContext
import com.ibm.analytics.wml.utils.clients.http._
import com.ibm.analytics.wml.utils.clients.http.models.HttpCredentialsProvider
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging.reqId
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec
import spray.json._

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class GlobalAssetTypesSpec extends AnyWordSpec with StrictLogging {
  private val testWaitDuration: FiniteDuration = 1.minutes
  private implicit val iamContext: IAMContext = IAMContext()

  private def getMessage(exception: Throwable): String = {
    exception.getMessage match {
      case msg: String =>
        if (msg != null)
          msg
        else
          exception.getClass.getName
      case _ =>
        exception.getClass.getName
    }
  }

  def getICPAuthV1(config: Config,
                   builder: HttpClientBuilder): Future[String] = {
    val token = Base64.getEncoder.encodeToString(s"${config.getString("icp.token.username")}:${config.getString("icp.token.password")}".getBytes("UTF-8"))
    Future.successful(s"Basic $token")
  }

  def getICPAuthV2(config: Config,
                   builder: HttpClientBuilder)
                  (implicit system: ActorSystem): Future[String] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val url = Uri(config.getString("icp.token.url"))
    val validateUrl = url.withPath(Uri.Path("/v1/preauth/validateAuth"))
    val client = builder.get(
      url.authority.host.address(),
      if (url.authority.port > 0) url.authority.port else 443,
      HttpClientBuilder.Keys.CAMS_CLIENT
    )
    val credentialsProvider = HttpCredentialsProvider(BasicHttpCredentials(config.getString("icp.token.username"), config.getString("icp.token.password")))
    for {
      response <- client.get(
        uri = validateUrl.toString(),
        action = Some("get-icp-token")
      )(Some(credentialsProvider), RetryFailure(3))
      json <- Unmarshal(response.entity).to[String]
      token <- Future.fromTry {
        Try {
          val js = JsonParser(json).asJsObject
          js.fields.get("accessToken") match {
            case Some(token) =>
              token match {
                case JsString(token) =>
                  token
                case _ =>
                  fail(s"Wrong type for 'accessToken' in $json")
              }
            case None =>
              fail(s"Failed to to find 'accessToken' in $json")
          }
        }
      }
    } yield {
      info(s"Using ICP token: $token")
      s"Bearer $token"
    }
  }

  def getIAMAuth(config: Config): Future[String] = {
    Future.successful(config.getString("iam.apikey"))
  }

  "GlobalAssetTypes" when {
    implicit val system: ActorSystem = ActorSystem("test-global-asset-types")
    implicit val ec: ExecutionContext = system.dispatcher

    def reportingFunc(metric: HttpMetric): Unit = {
      def toString(metric: HttpMetric): String = {
        val report = JsObject(
          Map(
            "host" -> JsString(metric.host),
            "port" -> JsNumber(metric.port),
            "method" -> JsString(metric.method),
            "uri" -> JsString(metric.uri),
            "startTime" -> JsString(formatAsDate(metric.startTime))
          ) ++ (if (metric.statusCode.isDefined) Map("statusCode" -> JsNumber(metric.statusCode.get)) else Map())
            ++ (if (metric.duration > 0) Map("duration" -> JsNumber(metric.duration)) else Map())
            ++ (if (metric.action.isDefined) Map("statusCode" -> JsString(metric.action.get)) else Map())
            ++ (if (metric.origin.isDefined) Map("statusCode" -> JsString(metric.origin.get)) else Map())
            ++ (if (metric.service.isDefined) Map("statusCode" -> JsString(metric.service.get)) else Map())
        )
        logPrint(js = report, prettyPrint = Some(false))
      }

      reqId(metric.contextId)(() => logger.info(toString(metric)))
    }

    val clientBuilder = Try(
      CachedHttpClientBuilder(
        service = Some(this.getClass.getSimpleName),
        allowAllCerts = true,
        reporting = reportingFunc
      )(system)
    )

    def builder: HttpClientBuilder = {
      clientBuilder match {
        case Success(builder) => builder
        case Failure(exception) => fail(s"Failed to create HTTP client builder: ${exception.getMessage}")
      }
    }

    def toCommandLine(args: Array[String]): CommandLine = {
      CommandLine(args)
    }

    def getAuth(platform: String): String = {
      Await.result(if ("ICP".equalsIgnoreCase(platform)) getICPAuthV1(config, builder) else getIAMAuth(config), testWaitDuration)
    }

    "creating an app with null should fail" in {
      Try(toCommandLine(null)) match {
        case Success(_) =>
          fail("App should fail with bad command line args")
        case Failure(exception) =>
          exception match {
            case _: CommandLineArgsException => // ok
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception ${getMessage(e)}", e)
          }
      }
    }

    "creating an app with no args should fail" in {
      val emptyArgs: Array[String] = Array()
      Try(toCommandLine(emptyArgs)) match {
        case Success(_) =>
          fail("App should fail with bad command line args")
        case Failure(exception) =>
          exception match {
            case _: CommandLineArgsException => // ok
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception ${getMessage(e)}", e)
          }
      }
    }

    "creating an app with too many args should fail" in {
      val tooManyArgs: Array[String] = Array("TEST", "host", "auth", "not-expected")
      Try(toCommandLine(tooManyArgs)) match {
        case Success(_) =>
          fail("App should fail with bad command line args")
        case Failure(exception) =>
          exception match {
            case _: CommandLineArgsException => // ok
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception ${getMessage(e)}", e)
          }
      }
    }

    "creating an app with an empty arg should fail" in {
      val emptyArg: Array[String] = Array("TEST", "", "auth")
      Try(toCommandLine(emptyArg)) match {
        case Success(_) =>
          fail("App should fail with bad command line args")
        case Failure(exception) =>
          exception match {
            case _: CommandLineArgsException => // ok
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception ${getMessage(e)}", e)
          }
      }
    }

    "creating an app with bad url should fail" in {
      val badArgs: Array[String] = Array("TEST", "/ml/v4/", "auth")
      Try(toCommandLine(badArgs)) match {
        case Success(_) =>
          fail("App should fail with bad command line args")
        case Failure(exception) =>
          exception match {
            case _: CommandLineArgsException => // ok
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception ${getMessage(e)}", e)
          }
      }
    }

    "getting the authentication details with bad API key should fail" in {
      // this fails with bad API key on cloud because the first thing that
      // it does is to look up the IAM token from the
      val badHostName: Array[String] = Array("TEST", "https://notused", "auth")
      val commands: CommandLine = Try(toCommandLine(badHostName)) match {
        case Success(commands) => commands
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception ${getMessage(exception)}", exception)
      }
      Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, NoRetry), testWaitDuration)) match {
        case Success(_) =>
          fail("App should fail with bad API key")
        case Failure(exception) =>
          exception match {
            case e: AuthenticationException =>
              info(s"Bad host error message: ${e.getMessage}")
              assert(e.getMessage.contains("API key"))
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception when getting credentials ${getMessage(e)}", e)
          }
      }
    }

    "getting the authentication details should succeed" in {
      val goodArgs: Array[String] = Array(
        "TEST",
        "https://notused",
        Await.result(getIAMAuth(config), testWaitDuration)
      )
      val commands: CommandLine = Try(toCommandLine(goodArgs)) match {
        case Success(commands) =>
          commands
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception ${getMessage(exception)}", exception)
      }
      info(s"Getting auth details for ${commands.auth}")
      Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, RetryFailure(3)), 15.seconds)) match {
        case Success(auth) =>
          info(s"Auth headers: ${auth.toString()}")
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception when getting credentials ${getMessage(exception)}", exception)
      }
    }

    "creating the global asset details should fail with bad host" in {
      val testWaitDuration = 20.seconds
      val goodArgs: Array[String] = Array(
        "TEST",
        "https://badhost",
        Await.result(getIAMAuth(config), testWaitDuration)
      )
      val commands: CommandLine = Try(toCommandLine(goodArgs)) match {
        case Success(commands) =>
          commands
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception ${getMessage(exception)}", exception)
      }
      val authDetails: Map[String, String] = Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, NoRetry), testWaitDuration)) match {
        case Success(auth) => auth
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception when getting credentials ${getMessage(exception)}", exception)
      }
      Try(
        Await.result(
          GlobalAssetTypesCreator(
            commands.serviceUrl,
            authDetails,
            commands.listOnly,
            commands.forceUpdate,
            builder,
            NoRetry
          ).declareGlobalAssetTypes(None), testWaitDuration
        )
      ) match {
        case Success(_) =>
          fail("App should fail with bad command line args")
        case Failure(exception) =>
          exception match {
            case e: GlobalAssetTypesException =>
              // ok
              info(s"Error message for bad host: ${e.getMessage}")
            case e: TimeoutException =>
              // ok
              info(s"Error message for bad host: ${e.getMessage}")
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception ${getMessage(e)}", e)
          }
      }
    }

    "getting help" in {
      val goodArgs: Seq[Array[String]] = Seq(
        Array(
          "--help"
        ),
        Array(
          "ICP",
          "--usage"
        )
      )
      for (args <- goodArgs) {
        Try(GlobalAssetTypesApp.main(args)) match {
          case Success(_) => // ok
          case Failure(exception) =>
            exception match {
              case e: Throwable =>
                logger.error(getMessage(e), e)
                fail(s"Unexpected exception when listing global assets ${getMessage(e)}", e)
            }
        }
      }
    }

    "listing the global asset details should succeed" ignore {
      val platform = "icp"
      val auth: String = getAuth(platform)
      val goodArgs: Array[String] = Array(
        "TEST",
        config.getString(s"${platform.toLowerCase}.cluster.url"),
        auth,
        "--list-only",
        "--no-exit"
      )
      Try(GlobalAssetTypesApp.main(goodArgs)) match {
        case Success(_) => // ok
        case Failure(exception) =>
          exception match {
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception when listing global assets ${getMessage(e)}", e)
          }
      }
      // now see what status we get back
      val commands: CommandLine = Try(toCommandLine(goodArgs)) match {
        case Success(commands) =>
          commands
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception ${getMessage(exception)}", exception)
      }
      val authDetails: Map[String, String] = Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, NoRetry), testWaitDuration)) match {
        case Success(auth) => auth
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception when getting credentials ${getMessage(exception)}", exception)
      }

      def getStatus(status: AssetStatus): String = {
        (status.oldState, status.newState) match {
          case (UpToDate, UpToDate) =>
            s"Asset ${status.name} ${status.newVersion} is ${UpToDate.name}"
          case (old, UpToDate) =>
            s"Asset ${status.name} ${status.newVersion} was updated because the old state was '${old.name}'"
          case (old, _) =>
            s"Asset ${status.name} ${status.newVersion} needs to be updated because old state is ${old.name}"
        }
      }

      Try(
        Await.result(
          GlobalAssetTypesCreator(
            commands.serviceUrl,
            authDetails,
            commands.listOnly,
            commands.forceUpdate,
            builder,
            RetryFailure(3)
          ).declareGlobalAssetTypes(Some("list-global-asset-types")), testWaitDuration
        )
      ) match {
        case Success(status) =>
          val stats: List[AssetStatus] = status.toJson.convertTo[List[AssetStatus]]
          info(s"""Got list status from ${commands.serviceUrl}: ${stats.map(stat => getStatus(stat)).mkString("\n  ", "\n  ", "")}""")
        case Failure(exception) =>
          exception match {
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception when listing global assets ${getMessage(e)}", e)
          }
      }
    }

    "creating the global asset details should succeed" ignore {
      val platform = "icp"
      val env = config.getString(s"${platform.toLowerCase}.client.auth")
      val auth: String = getAuth(platform)
      val goodArgs: Array[String] = Array(
        env,
        config.getString(s"${platform.toLowerCase}.cluster.url"),
        auth
      )
      val commands: CommandLine = Try(toCommandLine(goodArgs)) match {
        case Success(commands) =>
          commands
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception ${getMessage(exception)}", exception)
      }
      val authDetails: Map[String, String] = Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, NoRetry), testWaitDuration)) match {
        case Success(auth) => auth
        case Failure(exception) =>
          logger.error(getMessage(exception), exception)
          fail(s"Unexpected exception when getting credentials ${getMessage(exception)}", exception)
      }
      Try(
        Await.result(
          GlobalAssetTypesCreator(
            commands.serviceUrl,
            authDetails,
            commands.listOnly,
            commands.forceUpdate,
            builder,
            RetryFailure(3)
          ).declareGlobalAssetTypes(Some("register-global-asset-types")), testWaitDuration
        )
      ) match {
        case Success(status) =>
          info(s"Got update status from ${commands.serviceUrl}: ${status.toJson.prettyPrint}")
        case Failure(exception) =>
          exception match {
            case e: Throwable =>
              logger.error(getMessage(e), e)
              fail(s"Unexpected exception when updating global assets: ${getMessage(e)}", e)
          }
      }
    }

    "listing the asset types with the WML service id should succeed" in {
      if (StableServiceId.verifyEnvironment.isFailure) {
        info(s"The WML service id is not available - test is DISABLED")
      } else {
        val goodArgs: Array[String] = Array(
          "TEST",
          "https://api.dataplatform.dev.cloud.ibm.com",
          sys.env("WML_STABLE_API_KEY"),
          "--list-only",
          "--request-id=list-assets-with-iam-apikey",
          "--no-exit"
        )
        // now see what status we get back
        val commands: CommandLine = Try(toCommandLine(goodArgs)) match {
          case Success(commands) =>
            commands
          case Failure(exception) =>
            logger.error(getMessage(exception), exception)
            fail(s"Unexpected exception ${getMessage(exception)}", exception)
        }
        val authDetails: Map[String, String] = Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, NoRetry), testWaitDuration)) match {
          case Success(auth) => auth
          case Failure(exception) =>
            logger.error(getMessage(exception), exception)
            fail(s"Unexpected exception when getting credentials ${getMessage(exception)}", exception)
        }

        def getStatus(status: AssetStatus): String = {
          (status.oldState, status.newState) match {
            case (UpToDate, UpToDate) =>
              s"Asset ${status.name} ${status.newVersion} is ${UpToDate.name}"
            case (old, UpToDate) =>
              s"Asset ${status.name} ${status.newVersion} was updated because the old state was '${old.name}'"
            case (old, _) =>
              s"Asset ${status.name} ${status.newVersion} needs to be updated because old state is ${old.name}"
          }
        }

        Try(
          Await.result(
            GlobalAssetTypesCreator(
              commands.serviceUrl,
              authDetails,
              commands.listOnly,
              commands.forceUpdate,
              builder,
              RetryFailure(3)
            ).declareGlobalAssetTypes(commands.requestId),
            testWaitDuration
          )
        ) match {
          case Success(status) =>
            val stats: List[AssetStatus] = status.toJson.convertTo[List[AssetStatus]]
            info(s"""Got list status from ${commands.serviceUrl}: ${stats.map(stat => getStatus(stat)).mkString("\n  ", "\n  ", "")}""")
          case Failure(exception) =>
            exception match {
              case e: Throwable =>
                logger.error(getMessage(e), e)
                fail(s"Unexpected exception when listing global assets ${getMessage(e)} with commands $commands", e)
            }
        }
      }
    }

    "registering the asset types with the WML service id should succeed" ignore {
      if (StableServiceId.verifyEnvironment.isFailure) {
        info(s"The WML service id is not available - test is DISABLED")
      } else {
        val goodArgs: Array[String] = Array(
          "TEST",
          "https://api.dataplatform.dev.cloud.ibm.com",
          sys.env("WML_STABLE_API_KEY"),
          "--request-id=register-assets-with-iam-apikey",
          "--no-exit"
        )
        // now see what status we get back
        val commands: CommandLine = Try(toCommandLine(goodArgs)) match {
          case Success(commands) =>
            commands
          case Failure(exception) =>
            logger.error(getMessage(exception), exception)
            fail(s"Unexpected exception ${getMessage(exception)}", exception)
        }
        val authDetails: Map[String, String] = Try(Await.result(CAMSAuthenticator.getAuthDetails(commands, builder, NoRetry), testWaitDuration)) match {
          case Success(auth) => auth
          case Failure(exception) =>
            logger.error(getMessage(exception), exception)
            fail(s"Unexpected exception when getting credentials ${getMessage(exception)} with commands $commands", exception)
        }

        def getStatus(status: AssetStatus): String = {
          (status.oldState, status.newState) match {
            case (UpToDate, UpToDate) =>
              s"Asset ${status.name} ${status.newVersion} is ${UpToDate.name}"
            case (old, UpToDate) =>
              s"Asset ${status.name} ${status.newVersion} was updated because the old state was '${old.name}'"
            case (old, _) =>
              s"Asset ${status.name} ${status.newVersion} needs to be updated because old state is ${old.name}"
          }
        }

        Try(
          Await.result(
            GlobalAssetTypesCreator(
              commands.serviceUrl,
              authDetails,
              commands.listOnly,
              commands.forceUpdate,
              builder,
              RetryFailure(3)
            ).declareGlobalAssetTypes(commands.requestId), testWaitDuration
          )
        ) match {
          case Success(status) =>
            val stats: List[AssetStatus] = status.toJson.convertTo[List[AssetStatus]]
            info(s"""Got update status from ${commands.serviceUrl}: ${stats.map(stat => getStatus(stat)).mkString("\n  ", "\n  ", "")}""")
          case Failure(exception) =>
            exception match {
              case e: Throwable =>
                logger.error(getMessage(e), e)
                fail(s"Unexpected exception when registering global assets ${getMessage(e)} with commands $commands", e)
            }
        }
      }
    }
  }
}
