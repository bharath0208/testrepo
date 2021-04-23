/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.cloudant

import java.net.URL
import java.nio.file.{Files, StandardCopyOption}
import java.util.UUID

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{StatusCode, StatusCodes}
import com.cloudant.client.api.model.Response
import com.cloudant.client.api.views.Key
import com.cloudant.client.api.{ClientBuilder, CloudantClient, Database, DesignDocumentManager}
import com.cloudant.client.org.lightcouch.{DocumentConflictException, NoDocumentException}
import com.google.gson.{JsonObject, JsonParser}
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.ibm.ml.repository.v4.utils.{ServiceException, logPrint}
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging
import spray.json.{DefaultJsonProtocol, JsValue, _}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object SimpleCloudantClient extends StrictLogging {

  def getDBConfig(config: Config): (String, String, String, String) = {
    Try {
      val host = config.getString("service.cloudant.host")
      val port = config.getString("service.cloudant.port")
      val username = config.getString("service.cloudant.username")
      val password = config.getString("service.cloudant.password")
      val dbName = config.getString("service.cloudant.dbName")
      (s"https://$host:$port", username, password, dbName)
    } match {
      case Success(result) =>
        result
      case Failure(e) =>
        ExceptionLogger.log(Some(s"Failed to read cloudant config from 'service.cloudant' due to ${e.getLocalizedMessage}"), e, None, callStack = true)
        throw e
    }
  }

  def handleException[T](f: => T): T = {
    Try {
      f
    } match {
      case Success(v) => v
      case Failure(exception) =>
        exception match {
          case se: ServiceException => throw se
          case dce: DocumentConflictException =>
            throw new ServiceException(
              status = StatusCodes.Conflict.intValue,
              code = dce.getError,
              message = dce.getReason,
              moreInfo = None,
              cause = None,
              downstreamFailures = None,
              target = None
            )
          case nde: NoDocumentException =>
            throw new ServiceException(
              status = StatusCodes.NotFound.intValue,
              code = nde.getError,
              message = nde.getReason,
              moreInfo = None,
              cause = None,
              downstreamFailures = None,
              target = None
            )
          case t: Throwable =>
            throw new ServiceException(
              status = StatusCodes.InternalServerError.intValue,
              code = "unknown_cloudant_error",
              message = t.getLocalizedMessage,
              moreInfo = None,
              cause = None,
              downstreamFailures = None,
              target = None
            )
        }
    }
  }
}

case class SimpleCloudantClient(dbUrl: String,
                                dbName: String,
                                username: String,
                                password: String,
                                designDocPath: Option[String] = None,
                                ignoreCerts: Boolean = false)(implicit system: ActorSystem) extends StrictLogging {
  private implicit val ec: ExecutionContext = system.dispatcher

  val DESIGN_DOC_ID = "design_doc"

  // connect to the cloudant
  val client: CloudantClient = Try {
    logger.info("Initializing Cloudant client...")
    val builder = ClientBuilder.url(new URL(dbUrl))
      .username(username)
      .password(password)

    if (ignoreCerts) builder.disableSSLAuthentication().build()
    else builder.build()

  } match {
    case Success(c) =>
      logger.info(s"Created Cloudant client using $dbUrl")
      c
    case Failure(e) =>
      ExceptionLogger.log(Some(s"Failed to create Cloudant client using $dbUrl due to ${e.getLocalizedMessage}"), e, None, callStack = true)
      throw e
  }

  // Get a Database instance to interact with, create the DB if it does not exist
  val db: Database = Try {
    logger.info(s"Connecting to Cloudant DB $dbName...")
    client.database(dbName, true)
  } match {
    case Success(c) =>
      logger.info(s"Connected to Cloudant DB $dbName")
      c
    case Failure(e) =>
      ExceptionLogger.log(Some(s"Failed to connect to Cloudant DB $dbName due to ${e.getLocalizedMessage}"), e, None, callStack = true)
      throw e
  }

  //initialize design doc
  designDocPath.map { path => {
    Try {
      logger.info(s"Updating design doc for Cloudant DB $dbName...")
      logger.info(s"loading file $path")
      // read design doc from resource
      val inputStream = ClassLoader.getSystemClassLoader.getResourceAsStream(path)
      val tempPath = Files.createTempFile("desdoc-", ".json")
      Files.copy(inputStream, tempPath, StandardCopyOption.REPLACE_EXISTING)
      val fileDesignDoc = DesignDocumentManager.fromFile(tempPath.toFile)

      // try set the Revision for the new file doc for the json compare
      try {
        val dbDesignDoc = db.getDesignDocumentManager.get(DESIGN_DOC_ID)
        fileDesignDoc.setRevision(dbDesignDoc.getRevision)
      } catch {
        case _: Throwable =>
      }
      // update the design doc, if the the doc is same it will not update
      db.getDesignDocumentManager.put(fileDesignDoc)
      // delete the temp file
      Files.delete(tempPath)
    } match {
      case Success(_) =>
        logger.info(s"Updated design doc for Cloudant DB $dbName")
      case Failure(e) =>
        ExceptionLogger.log(Some(s"Failed to update design doc for Cloudant DB $dbName"), e, None, callStack = true)
        throw e
    }
  }
  }

  // public function

  def save(payload: JsValue): Future[JsValue] = Future {
    SimpleCloudantClient.handleException {
      val json = new JsonParser().parse(payload.compactPrint).getAsJsonObject
      val uuid = UUID.randomUUID
      val randomUUIDString = uuid.toString
      json.addProperty("_id", randomUUIDString)
      val response = db.save(json)
      checkResponse(payload, response)
    }
  }

  def update(payload: JsValue): Future[JsValue] = Future {
    SimpleCloudantClient.handleException {
      val json = new JsonParser().parse(payload.compactPrint).getAsJsonObject
      logger.trace(s"Cloudant doc to be updated:\n${logPrint(payload)}")
      val response = db.update(json)
      checkResponse(payload, response)
    }
  }

  def remove(payload: JsValue): Future[Unit] = Future {
    SimpleCloudantClient.handleException {
      val json = new JsonParser().parse(payload.compactPrint).getAsJsonObject
      val response = db.remove(json)
      checkResponse(payload, response)
    }
  }

  def remove(id: String, rev: String): Future[Unit] = Future {
    SimpleCloudantClient.handleException {
      db.remove(id, rev)
    }
  }

  def find(id: String): Future[JsValue] = Future {
    SimpleCloudantClient.handleException {
      val json = db.find(classOf[JsonObject], id)
      val result = json.toString.parseJson
      logger.trace(s"Get cloudant doc result:\n${logPrint(result)}")
      result
    }
  }

  def view(viewName: String, keys: Option[List[String]]): Future[JsValue] = Future {
    SimpleCloudantClient.handleException {
      val view = db.getViewRequestBuilder(DESIGN_DOC_ID, viewName)
        .newRequest(Key.Type.STRING, classOf[JsonObject])
      keys.map(
        view.keys(_: _*)
      ).getOrElse(view).build().getResponse().getValues.toString.parseJson
    }
  }

  private def checkResponse(payload: JsValue, resp: Response): JsValue = {
    val statusCode = resp.getStatusCode
    if (StatusCode.int2StatusCode(statusCode).isSuccess) {
      logger.debug(s"Cloudant operation successfully executed. Status code: $statusCode")
      val newFields = Map("_id" -> JsString(resp.getId), "_rev" -> JsString(resp.getRev))
      val oldFields = payload.asJsObject.fields
      import DefaultJsonProtocol._
      (oldFields ++ newFields).toJson
    } else {
      val error = if (resp.getError != null) resp.getError else "unknown_cloudant_error"
      val reason = if (resp.getReason != null) resp.getReason else s"Unknown cloudant error with status code $statusCode"
      logger.debug(s"Cloudant operation fail executed. Status code: $statusCode, error: $error, reason: $reason")
      throw new ServiceException(
        status = statusCode,
        code = error,
        message = reason,
        moreInfo = None,
        cause = None,
        downstreamFailures = None,
        target = None
      )
    }
  }
}
