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

import java.io.Closeable

import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetInput, Assets}
import com.ibm.analytics.environments.api.v2.hardware_spec.HardwareSpecJsonFormat._
import com.ibm.analytics.environments.api.v2.hardware_spec.{HardwareSpecification, HardwareSpecificationResource, HardwareSpecificationResources}
import com.ibm.analytics.environments.api.v2.software_spec.SoftwareSpecJsonFormat._
import com.ibm.analytics.environments.api.v2.software_spec.{SoftwareSpecification, SoftwareSpecificationEntity, SoftwareSpecifications}
import com.ibm.analytics.spaces.api.v2.SpacesJsonFormat._
import com.ibm.analytics.spaces.api.v2.{SpaceEntityRequest, SpaceResource, SpaceResources}
import com.ibm.analytics.wml.api.v4.spaces.{SpacesEntity, SpacesResource, SpacesResources}
import com.ibm.ml.repository.v4.tests.utils
import com.typesafe.config.Config
import org.apache.http._
import org.apache.http.client.methods._
import org.apache.http.client.utils.URIBuilder
import org.apache.http.entity.{ByteArrayEntity, ContentType, StringEntity}
import org.apache.http.impl.client.CloseableHttpClient
import org.apache.http.util.EntityUtils
import org.apache.logging.log4j.{LogManager, Logger}
import org.scalatest.Assertions._
import spray.json.{JsString, _}
import com.ibm.ml.repository.v4.tests.utils.V4TestServicesClient._

import scala.collection.mutable
import scala.math.min
import scala.util.{Failure, Success, Try}

trait HttpTestClient {
  private val logger: Logger = LogManager.getLogger(classOf[HttpTestClient])
  val httpClient: CloseableHttpClient

  private val TEST_ENTITY_HEADER = "__TEST_HACK__"
  private val TOKEN_PRINT_LENGTH = 40

  def addQueryParameters(uri: String, query: Iterable[(String, String)] = None): String = {
    if (query.nonEmpty) {
      val builder: URIBuilder = new URIBuilder(uri)
      for ((k, v) <- query) {
        builder.addParameter(k, v)
      }
      builder.build().toString
    } else
      uri
  }

  def trim(value: String, len: Int): String = {
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
    s.append(getHeader(request, V4TestServicesClient.HEADER_REQUEST_ID, Some(" "), None))
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

  def logRequestId(request: HttpRequestBase): String = {
    if (request.getHeaders(V4TestServicesClient.HEADER_REQUEST_ID).length > 0) {
      s"[${request.getFirstHeader(V4TestServicesClient.HEADER_REQUEST_ID).getValue}]"
    } else
      ""
  }

  def log(request: HttpRequestBase): Long = {
    logger.trace(s"Request: ${logRequestId(request)}${serialize(request)}")
    System.currentTimeMillis()
  }

  def log(response: HttpResponse, request: HttpRequestBase, start: Long): Unit = {
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

  private def checkResponseHeaders(request: HttpRequestBase, response: HttpResponse): Unit = {
    //if (response.getStatusLine.getStatusCode == 404) {
    // we don't expect anything back
    // } else if (response.getStatusLine.getStatusCode == 415) {
    // we get back a message from akka: The request's Content-Type is not supported. Expected: application/json
    //} else
    if (response.getStatusLine.getStatusCode >= 400) {
      getEntity(response) match {
        case Some(json) =>
          Try {
            JsonParser(json).asJsObject
          } match {
            case Success(jss) =>
              assert(jss.fields.contains("trace"))
              val trace: String = jss.fields("trace").asInstanceOf[JsString].value
              val reqIdHeader = request.getFirstHeader(V4TestServicesClient.HEADER_REQUEST_ID)
              if (reqIdHeader != null) {
                // assert(reqIdHeader.getValue.equalsIgnoreCase(trace), s"Request ${reqIdHeader.getValue} != response $trace")
                if (!reqIdHeader.getValue.equalsIgnoreCase(trace))
                  logger.warn(s"Request ${reqIdHeader.getValue} != response $trace for ${request.getMethod} ${request.getURI}")
              } else
                logger.warn(s"No requestId in request ${request.getURI}", new Exception("call-stack"))
            case Failure(exception) =>
              logger.info(s"Response is not json: $json ($exception)")
          }
        case None =>
          fail(s"Failed to get error reply for request ${serialize(request)} : ${serialize(response)}")
      }
    } else {
      if (request.getHeaders(V4TestServicesClient.HEADER_REQUEST_ID).length > 0) {
        val responseHeader = V4TestServicesClient.HEADER_GLOBAL_TRANSACTION_ID // HEADER_REQUEST_ID
        if (request.getURI.getPath.toLowerCase.startsWith("/v4/learning_systems")) {
          assert(response.getHeaders(responseHeader).length > 0, s"No requestId ($responseHeader) for response ${request.getURI} : ${serialize(response)}")
          assert(response.getHeaders(responseHeader).length == 1, s"""Found ${response.getHeaders(responseHeader).length} $responseHeader headers (too many)""")
          assert(request.getFirstHeader(V4TestServicesClient.HEADER_REQUEST_ID).getValue.equalsIgnoreCase(response.getFirstHeader(responseHeader).getValue))
        } else {
          if (response.getHeaders(responseHeader).length < 1) {
            logger.info(s"No requestId ($responseHeader) for response ${request.getURI} : ${serialize(response)}")
          } else {
            if (response.getHeaders(responseHeader).length > 1) logger.warn(s"""Found ${response.getHeaders(responseHeader).length} $responseHeader headers (too many)""")
            /* TODO later when x-global-transaction-id is supported by everyone
            if (!request.getFirstHeader(V4TestServicesClient.HEADER_REQUEST_ID).getValue.equalsIgnoreCase(response.getFirstHeader(responseHeader).getValue))
              logger.info(s"${request.getFirstHeader(V4TestServicesClient.HEADER_REQUEST_ID).getValue} != ${response.getFirstHeader(responseHeader).getValue} for response ${request.getURI} : ${serialize(response)}")
             */
          }
        }
      }
    }
  }

  // these are the methods for making HTTP calls

  def execute(request: HttpRequestBase,
              doLog: Boolean = true,
              checkResponse: Boolean = true): CloseableHttpResponse = {
    val start = if (doLog) log(request) else System.currentTimeMillis()
    val response = httpClient.execute(request)
    if (doLog)
      log(response, request, start)
    if (checkResponse)
      checkResponseHeaders(request, response)
    response
  }

  // this must be used to close a response when done
  def close(closeable: Closeable): Unit = {
    if (closeable != null) {
      try {
        closeable.close()
      }
      catch {
        case _: Exception => // ignore
      }
    }
  }

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
}

abstract class AbstractTestServicesClient(serviceUrl: String,
                                clusterUrl: String,
                                httpClient: CloseableHttpClient,
                                config: Config,
                                version: String,
                                clientAuth: String,
                                containerType: String,
                                authHeaders: Option[Map[String, String]] = None,
                                apiKey: String,
                                authTokenType: String) extends HttpTestClient {
  private val logger: Logger = LogManager.getLogger(classOf[V4TestServicesClient])

  val SPACES = "/v2/spaces"
  val PROJECTS_TRANSACTIONAL = "/transactional/v2/projects"
  val PROJECTS = "/v2/projects"
  val SOFTWARE_SPECIFICATIONS = "/v2/software_specifications"
  val HARDWARE_SPECIFICATIONS = "/v2/hardware_specifications"
  val DATA_ASSETS = "/v2/data_assets"
  val CAMS_ASSETS = "/v2/assets"
  val REVISIONS_URI = "/revisions"
  val CAMS_ASSET_ATTACHMENT = "/attachments"

  def serviceEndpoint: String = serviceUrl

  def clusterEndpoint: String = clusterUrl

  def iamTokenGenerator: IAMTokenGenerator = utils.GetIamToken(httpClient)

  private lazy val getIcpToken: GetIcpToken = utils.GetIcpToken(httpClient)

  private lazy val authenticationHeaders: Map[String, String] = {
    if (clientAuth.equalsIgnoreCase("ICP")) {
      if(authTokenType == "basic") {
        getIcpToken.getBasicAuthenticationHeaders
      } else {
        getIcpToken.getAuthenticationHeaders
      }
    } else {
      iamTokenGenerator.getAuthenticationHeaders(apiKey)
    }
  }

  def getAuthenticationHeaders(basicAuthSupported: Boolean, serviceIdTokenSupported: Boolean, uaaTokenRequired: Boolean): Map[String, String] = {
    val headers = authHeaders match {
      case Some(headers) => headers
      case None =>
        if(clientAuth.equalsIgnoreCase("ICP") && authTokenType == "basic" && !basicAuthSupported) {
          getIcpToken.getAuthenticationHeaders
        } else if (clientAuth.equalsIgnoreCase("IAM") && uaaTokenRequired) {
          utils.GetIamUAATokens(httpClient).getAuthenticationHeaders(apiKey) //apiKey does not matter
        } else if (clientAuth.equalsIgnoreCase("IAM") && authTokenType == "service-id" && !serviceIdTokenSupported) {
          val userApiKey = Try(config.getString(CONFIG_IAM_API_KEY)).getOrElse(throw new IllegalArgumentException(s"$CONFIG_IAM_API_KEY environment variable not set"))
          iamTokenGenerator.getAuthenticationHeaders(userApiKey)
        } else {
          authenticationHeaders
        }

    }
    logger.debug(s"Using authentication headers ${headers.mkString(",")}")
    headers
  }

  def getRequestIdHeaders(requestId: Option[String]): Map[String, String] = {
    requestId match {
      case Some(reqId) =>
        Map(V4TestServicesClient.HEADER_REQUEST_ID -> reqId)
      case None =>
        Map()
    }
  }

  private def isExpected(status: Int, expectedStatus: Iterable[Int]): Boolean = {
    if (expectedStatus.isEmpty)
      status < 400
    else {
      for (st <- expectedStatus)
        if (st == status)
          return true
      false
    }
  }

  private def handleRequest(request: HttpRequestBase,
                            expectedStatus: Iterable[Int] = None,
                            responseCanBeMissing: Boolean = false,
                            checkResponse: Boolean = true): (String, Array[Header], Int) = {
    val response = execute(request = request, checkResponse = checkResponse)
    try {
      val resp: Option[String] = getEntity(response)
      if (isExpected(response.getStatusLine.getStatusCode, expectedStatus))
        resp match {
          case Some(json) =>
            (json, response.getAllHeaders, response.getStatusLine.getStatusCode)
          case None =>
            if (responseCanBeMissing)
              (null, response.getAllHeaders, response.getStatusLine.getStatusCode)
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

  protected def getMethod(address: String,
                        queryParams: Map[String, String] = Map(),
                        extraHeaders: Map[String, String] = Map(),
                        expectedStatus: Iterable[Int] = None,
                        requestId: Option[String] = None,
                        checkResponse: Boolean = true,
                        authHeaders: Map[String, String]): (String, Array[Header], Int) = {
    val get = new HttpGet(addQueryParameters(address, queryParams))
    setHeaders(get, getRequestIdHeaders(requestId) ++ extraHeaders ++ authHeaders)
    acceptJson(get)
    handleRequest(request = get, expectedStatus = expectedStatus, checkResponse = checkResponse)
  }

  protected def putMethod(address: String,
                        entity: Option[HttpEntity],
                        queryParams: Map[String, String] = Map(),
                        extraHeaders: Map[String, String] = Map(),
                        expectedStatus: Iterable[Int] = None,
                        requestId: Option[String] = None,
                        checkResponse: Boolean = true,
                        authHeaders: Map[String, String]): (String, Array[Header], Int) = {
    val put = new HttpPut(addQueryParameters(address, queryParams))
    setHeaders(put, getRequestIdHeaders(requestId) ++ extraHeaders ++ authHeaders)
    acceptJson(put)
    entity match {
      case Some(en) =>
        put.setEntity(en)
      case None =>
        None
    }
    handleRequest(request = put, expectedStatus = expectedStatus, checkResponse = checkResponse, responseCanBeMissing = true)
  }

  protected def patchMethod(address: String,
                          json: String,
                          queryParams: Map[String, String] = Map(),
                          extraHeaders: Map[String, String] = Map(),
                          expectedStatus: Iterable[Int] = None,
                          requestId: Option[String] = None,
                          checkResponse: Boolean = true,
                          authHeaders: Map[String, String]): (String, Array[Header], Int) = {
    val patch = new HttpPatch(addQueryParameters(address, queryParams))
    setHeaders(patch, getRequestIdHeaders(requestId) ++ extraHeaders ++ authHeaders)
    acceptJson(patch)
    patch.setEntity(new StringEntity(json, ContentType.APPLICATION_JSON))
    handleRequest(request = patch, expectedStatus = expectedStatus, checkResponse = checkResponse)
  }

  protected def postMethod(address: String,
                         entity: Option[HttpEntity],
                         queryParams: Map[String, String] = Map(),
                         extraHeaders: Map[String, String] = Map(),
                         expectedStatus: Iterable[Int] = None,
                         requestId: Option[String] = None,
                         checkResponse: Boolean = true,
                         authHeaders: Map[String, String]): (String, Array[Header], Int) = {
    val post = new HttpPost(addQueryParameters(address, queryParams))
    setHeaders(post, getRequestIdHeaders(requestId) ++ extraHeaders ++ authHeaders)
    acceptJson(post)
    entity match {
      case Some(en) =>
        post.setEntity(en)
      case None =>
        None
    }
    handleRequest(request = post, expectedStatus = expectedStatus, checkResponse = checkResponse)
  }

  protected def deleteMethod(address: String,
                           queryParams: Map[String, String] = Map(),
                           extraHeaders: Map[String, String] = Map(),
                           expectedStatus: Iterable[Int] = None,
                           requestId: Option[String] = None,
                           checkResponse: Boolean = true,
                           authHeaders: Map[String, String]): (Option[String], Array[Header], Int) = {
    val delete = new HttpDelete(addQueryParameters(address, queryParams))
    setHeaders(delete, getRequestIdHeaders(requestId) ++ extraHeaders ++ authHeaders)
    val resp = handleRequest(request = delete, expectedStatus = expectedStatus, responseCanBeMissing = true, checkResponse = checkResponse)
    if (resp._1 == null)
      (None, resp._2, resp._3)
    else
      (Some(resp._1), resp._2, resp._3)
  }

  private def getAbsoluteUrl(uri: String): String = {
    if (!uri.toLowerCase.startsWith("http")) {
      val uriLC = uri.toLowerCase
      if (uriLC.contains("/v4/spaces")
        || uriLC.contains("/v2/software_specifications")
        || uriLC.contains("/v2/hardware_specifications")
        || uriLC.contains("/v2/data_assets")
        || uriLC.contains("/v2/projects")
        || uriLC.contains("/v2/spaces"))
        s"$clusterEndpoint$uri"
      else
        s"$serviceEndpoint$uri"
    } else
      uri
  }

  abstract class AbstractResource[EntityClass, ResourceClass, ResourceClasses](uri: String,
                                                                               convertResource: JsValue => ResourceClass,
                                                                               convertResources: JsValue => ResourceClasses,
                                                                               toJson: EntityClass => JsValue,
                                                                               resource: Class[_],
                                                                               resources: Class[_],
                                                                               useVersion: Boolean = true,
                                                                               basicAuthSupported: Boolean = true,
                                                                               serviceIdTokenSupported: Boolean = true,
                                                                               uaaTokenRequired: Boolean = false)
    extends V4TestService[EntityClass, ResourceClass, ResourceClasses] {

    def getServiceUri: String = uri

    def parseResource(value: JsValue): ResourceClass = {
      Try {
        convertResource(value)
      } match {
        case Success(resource) =>
          resource
        case Failure(exception) =>
          fail(s"Failed to convert ${value.prettyPrint} to ${resource.getSimpleName}: $exception")
      }
    }

    def parseResources(value: JsValue): ResourceClasses = {
      Try {
        convertResources(value)
      } match {
        case Success(resources) =>
          resources
        case Failure(exception) =>
          fail(s"Failed to convert ${value.prettyPrint} to ${resources.getSimpleName}: $exception")
      }
    }

    protected def jsonToEntity(json: JsValue): String = {
      // we send expanded json - easy to debug
      json.prettyPrint
    }

    def getAuthHeaders: Map[String, String] = {
      getAuthenticationHeaders(basicAuthSupported, serviceIdTokenSupported, uaaTokenRequired)
    }

    private def getUri(baseUri: String, parentId: Option[String], id: Option[String]): String = {
      val purl = parentId match {
        case Some(pId) =>
          assert(baseUri.contains("${parentId}"))
          baseUri.replace("${parentId}", pId)
        case None =>
          baseUri
      }
      val url = id match {
        case Some(i) => s"$purl/$i"
        case None => purl
      }
      getAbsoluteUrl(url)
    }

    private def checkParameters(url: String): String = {
      url
    }

    override def getUri(parentId: Option[String], id: Option[String]): String = {
      getUri(baseUri = getServiceUri, parentId = parentId, id = id)
    }

    protected def query(include: Option[String] = None,
                        assetId: Option[String] = None,
                        tagValue: Iterable[String] = None,
                        name: Option[String] = None,
                        spaceId: Option[String] = None,
                        projectId: Option[String] = None,
                        systemRuntimes: Option[Boolean] = None,
                        limit: Option[Int] = None,
                        finished: Option[Boolean] = None,
                        identity: Option[String] = None,
                        computeTotalCount: Option[Boolean] = None,
                        rev: Option[String] = None,
                        start: Option[String] = None,
                        contentFormat: Option[String] = None,
                       ): Map[String, String] = {
      val qp: mutable.Map[String, String] = mutable.Map()
      if (include.isDefined) qp += ("include" -> include.get)
      if (assetId.isDefined) qp += ("asset_id" -> assetId.get)
      // tag values are: tag.value=dl,spark
      if (tagValue.nonEmpty) qp += ("tag.value" -> tagValue.mkString(","))
      if (name.nonEmpty) qp += ("name" -> name.get)
      if (spaceId.isDefined) qp += ("space_id" -> spaceId.get)
      if (projectId.isDefined) qp += ("project_id" -> projectId.get)
      if (systemRuntimes.isDefined) qp += ("system_runtimes" -> systemRuntimes.get.toString)
      if (start.isDefined) qp += ("start" -> start.get)
      if (limit.isDefined) qp += ("limit" -> limit.get.toString)
      if (finished.isDefined) qp += ("finished" -> finished.get.toString)
      if (identity.isDefined) qp += ("identity" -> identity.get)
      if (computeTotalCount.isDefined) qp += ("total_count" -> computeTotalCount.get.toString)
      if (contentFormat.isDefined) qp += ("content_format" -> contentFormat.get)
      if (rev.isDefined) qp += ("rev" -> rev.get)
      if (useVersion) qp += ("version" -> version)
      qp.toMap
    }

    override def get(next: Option[String] = None,
                     include: Option[String] = None,
                     assetId: Option[String] = None,
                     tagValue: Iterable[String] = None,
                     spaceId: Option[String] = None,
                     projectId: Option[String] = None,
                     systemRuntimes: Option[Boolean] = None,
                     limit: Option[Int] = None,
                     finished: Option[Boolean] = None,
                     identity: Option[String] = None,
                     computeTotalCount: Option[Boolean] = None,
                     requestId: Option[String] = None,
                     parentId: Option[String] = None,
                     expectedStatus: Iterable[Int] = Some(200),
                     checkResponse: Boolean = true,
                     id: Option[String] = None): (ResourceClasses, Array[Header], Int) = {
      val address = next match {
        case Some(url) =>
          getUri(checkParameters(url), parentId, id)
        case None =>
          getUri(parentId, id)
      }
      val res = getMethod(
        address = address,
        queryParams = query(
          include = include,
          assetId = assetId,
          tagValue = tagValue,
          spaceId = spaceId,
          projectId = projectId,
          systemRuntimes = systemRuntimes,
          limit = limit,
          finished = finished,
          identity = identity,
          computeTotalCount = computeTotalCount
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (parseResources(JsonParser(res._1)), res._2, res._3)
    }

    override def getAsJson(include: Option[String] = None,
                           assetId: Option[String] = None,
                           tagValue: Iterable[String] = None,
                           spaceId: Option[String] = None,
                           projectId: Option[String] = None,
                           systemRuntimes: Option[Boolean] = None,
                           limit: Option[Int] = None,
                           finished: Option[Boolean] = None,
                           identity: Option[String] = None,
                           computeTotalCount: Option[Boolean] = None,
                           requestId: Option[String] = None,
                           parentId: Option[String] = None,
                           expectedStatus: Iterable[Int] = Some(200),
                           checkResponse: Boolean = true,
                           id: Option[String] = None): (JsValue, Array[Header], Int) = {
      val res = getMethod(
        address = getUri(parentId, id),
        queryParams = query(
          include = include,
          assetId = assetId,
          tagValue = tagValue,
          spaceId = spaceId,
          projectId = projectId,
          systemRuntimes = systemRuntimes,
          limit = limit,
          finished = finished,
          identity = identity,
          computeTotalCount = computeTotalCount
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (JsonParser(res._1), res._2, res._3)
    }

    override def create(entity: Option[EntityClass] = None,
                        requestId: Option[String] = None,
                        parentId: Option[String] = None,
                        spaceId: Option[String] = None,
                        projectId: Option[String] = None,
                        extraHeaders: Option[Map[String, String]] = None,
                        expectedStatus: Iterable[Int] = Some(201),
                        checkResponse: Boolean = true): (ResourceClass, Array[Header], Int) = {
      val en: Option[JsValue] = entity match {
        case Some(ec) => Some(toJson(ec))
        case None => None
      }
      val res = createJson(
        entity = en,
        requestId = requestId,
        parentId = parentId,
        spaceId = spaceId,
        projectId = projectId,
        extraHeaders = extraHeaders,
        expectedStatus = expectedStatus,
        checkResponse = checkResponse
      )
      (parseResource(res._1), res._2, res._3)
    }

    override def createJson(entity: Option[JsValue] = None,
                            requestId: Option[String] = None,
                            parentId: Option[String] = None,
                            spaceId: Option[String] = None,
                            projectId: Option[String] = None,
                            extraHeaders: Option[Map[String, String]] = None,
                            expectedStatus: Iterable[Int] = Some(201),
                            checkResponse: Boolean = true): (JsValue, Array[Header], Int) = {
      val en: Option[HttpEntity] = entity match {
        case Some(ec) => Some(new StringEntity(jsonToEntity(ec), ContentType.APPLICATION_JSON))
        case None => None
      }
      createRaw(
        entity = en,
        requestId = requestId,
        parentId = parentId,
        spaceId = spaceId,
        projectId = projectId,
        extraHeaders = extraHeaders,
        expectedStatus = expectedStatus,
        checkResponse = checkResponse
      )
    }

    override def createRaw(entity: Option[HttpEntity] = None,
                           requestId: Option[String] = None,
                           parentId: Option[String] = None,
                           spaceId: Option[String] = None,
                           projectId: Option[String] = None,
                           extraHeaders: Option[Map[String, String]] = None,
                           expectedStatus: Iterable[Int] = Some(201),
                           checkResponse: Boolean = true): (JsValue, Array[Header], Int) = {
      val uri = getUri(parentId, None)
      val res = postMethod(
        address = uri,
        queryParams = query(spaceId = spaceId, projectId = projectId),
        extraHeaders = if (extraHeaders.isDefined) extraHeaders.get else Map(),
        entity = entity,
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      if ((res._1 == null) || res._1.trim.isEmpty)
        throw new Exception(s"Failed to get response from request POST $uri")
      val jsValue: JsValue = Try {
        JsonParser(res._1)
      } match {
        case Success(js) => js
        case Failure(exception) =>
          throw new Exception(s"Failed to convert '${res._1}' to json: $exception", exception)
      }
      (jsValue, res._2, res._3)
    }

    override def getById(id: String,
                         rev: Option[String] = None,
                         spaceId: Option[String] = None,
                         projectId: Option[String] = None,
                         requestId: Option[String] = None,
                         parentId: Option[String] = None,
                         expectedStatus: Iterable[Int] = Some(200),
                         checkResponse: Boolean = true): (ResourceClass, Array[Header], Int) = {
      val res = getMethod(
        address = getUri(parentId, Some(id)),
        queryParams = query(spaceId = spaceId, projectId = projectId, rev = rev),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (parseResource(JsonParser(res._1)), res._2, res._3)
    }

    override def getByIdAsJson(id: String,
                               rev: Option[String] = None,
                               spaceId: Option[String] = None,
                               projectId: Option[String] = None,
                               requestId: Option[String] = None,
                               parentId: Option[String] = None,
                               expectedStatus: Iterable[Int] = Some(200),
                               checkResponse: Boolean = true): (JsValue, Array[Header], Int) = {
      val res = getMethod(
        address = getUri(parentId, Some(id)),
        queryParams = query(spaceId = spaceId, projectId = projectId, rev = rev),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (JsonParser(res._1), res._2, res._3)
    }

    override def getByHref(href: String,
                           spaceId: Option[String] = None,
                           projectId: Option[String] = None,
                           requestId: Option[String] = None,
                           parentId: Option[String] = None,
                           expectedStatus: Iterable[Int] = Some(200),
                           checkResponse: Boolean = true): (ResourceClass, Array[Header], Int) = {
      val res = getMethod(
        address = getUri(href, parentId, None),
        queryParams = query(spaceId = spaceId, projectId = projectId),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (parseResource(JsonParser(res._1)), res._2, res._3)
    }

    override def update(id: String,
                        patch: JsValue,
                        spaceId: Option[String] = None,
                        projectId: Option[String] = None,
                        requestId: Option[String] = None,
                        parentId: Option[String] = None,
                        expectedStatus: Iterable[Int] = Some(200),
                        checkResponse: Boolean = true): (ResourceClass, Array[Header], Int) = {
      val res = updateRaw(
        id = id,
        patch = patch,
        spaceId = spaceId,
        projectId = projectId,
        requestId = requestId,
        parentId = parentId,
        expectedStatus = expectedStatus,
        checkResponse = checkResponse
      )
      (parseResource(res._1), res._2, res._3)
    }

    override def updateRaw(id: String,
                           patch: JsValue,
                           spaceId: Option[String] = None,
                           projectId: Option[String] = None,
                           requestId: Option[String] = None,
                           parentId: Option[String] = None,
                           expectedStatus: Iterable[Int] = Some(200),
                           checkResponse: Boolean = true): (JsValue, Array[Header], Int) = {
      val res = patchMethod(
        address = getUri(parentId, Some(id)),
        queryParams = query(spaceId = spaceId, projectId = projectId),
        json = jsonToEntity(patch),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (JsonParser(res._1), res._2, res._3)
    }

    override def delete(id: String,
                        spaceId: Option[String] = None,
                        projectId: Option[String] = None,
                        requestId: Option[String] = None,
                        parentId: Option[String] = None,
                        expectedStatus: Iterable[Int] = Some(204),
                        checkResponse: Boolean = true,
                        queryParams: Map[String, String] = Map()): (Option[String], Array[Header], Int) = {
      deleteMethod(
        address = getUri(parentId, Some(id)),
        queryParams = query(spaceId = spaceId, projectId = projectId) ++ queryParams,
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
    }

    override def createRevisionRaw(id: String,
                                   payload: JsValue,
                                   spaceId: Option[String] = None,
                                   projectId: Option[String] = None,
                                   requestId: Option[String] = None,
                                   parentId: Option[String] = None,
                                   expectedStatus: Iterable[Int] = Some(200),
                                   extraHeaders: Option[Map[String, String]] = None,
                                   checkResponse: Boolean = true): (JsValue, Array[Header], Int) = {
      val uri = getUri(parentId, Some(id)) + REVISIONS_URI
      val res = postMethod(
        address = uri,
        queryParams = query(spaceId = spaceId, projectId = projectId),
        extraHeaders = if (extraHeaders.isDefined) extraHeaders.get else Map(),
        entity = Some(new StringEntity(jsonToEntity(payload), ContentType.APPLICATION_JSON)),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      if ((res._1 == null) || res._1.trim.isEmpty)
        throw new Exception(s"Failed to get response from request POST $uri")
      val jsValue: JsValue = Try {
        JsonParser(res._1)
      } match {
        case Success(js) => js
        case Failure(exception) =>
          throw new Exception(s"Failed to convert '${res._1}' to json: $exception", exception)
      }
      (jsValue, res._2, res._3)
    }

    override def createRevision(id: String,
                                payload: JsValue,
                                spaceId: Option[String] = None,
                                projectId: Option[String] = None,
                                requestId: Option[String] = None,
                                parentId: Option[String] = None,
                                expectedStatus: Iterable[Int] = Some(200),
                                extraHeaders: Option[Map[String, String]] = None,
                                checkResponse: Boolean = true): (ResourceClass, Array[Header], Int) = {
      val res = createRevisionRaw(id = id,
        payload = payload,
        spaceId = spaceId,
        projectId = projectId,
        requestId = requestId,
        parentId = parentId,
        expectedStatus = expectedStatus,
        extraHeaders = extraHeaders,
        checkResponse = checkResponse)

      (parseResource(res._1), res._2, res._3)
    }

    override def getRevisions(next: Option[String] = None,
                              spaceId: Option[String] = None,
                              projectId: Option[String] = None,
                              start: Option[String] = None,
                              limit: Option[Int] = None,
                              requestId: Option[String] = None,
                              parentId: Option[String] = None,
                              expectedStatus: Iterable[Int] = Some(200),
                              checkResponse: Boolean = true,
                              id: Option[String] = None): (JsValue, Array[Header], Int) = {
      val address = next match {
        case Some(url) =>
          getUri(checkParameters(url), parentId, id) + REVISIONS_URI
        case None =>
          getUri(parentId, id) + REVISIONS_URI
      }
      val res = getMethod(
        address = address,
        queryParams = query(
          spaceId = spaceId,
          projectId = projectId,
          limit = limit,
          start = start
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        checkResponse = checkResponse,
        authHeaders = getAuthHeaders
      )
      (JsonParser(res._1), res._2, res._3)
    }
  }

  abstract class AbstractResourceWithContent[EntityClass, ResourceClass, ResourceClasses](uri: String,
                                                                                          contentUri: String,
                                                                                          convertResource: JsValue => ResourceClass,
                                                                                          convertResources: JsValue => ResourceClasses,
                                                                                          toJson: EntityClass => JsValue,
                                                                                          resource: Class[_],
                                                                                          resources: Class[_],
                                                                                          useVersion: Boolean = true)
    extends AbstractResource[EntityClass, ResourceClass, ResourceClasses](
      uri = uri,
      convertResource = convertResource,
      convertResources = convertResources,
      toJson = toJson,
      resource = resource,
      resources = resources,
      useVersion = useVersion
    ) {
    def upload(id: String,
               spaceId: Option[String] = None,
               projectId: Option[String] = None,
               contents: Array[Byte],
               contentFormat: Option[String] = Some("native"),
               contentMIMEType: Option[String] = None,
               expectedStatus: Option[Int] = Some(204),
               requestId: Option[String] = None): (String, Array[Header], Int) = {
      val uri = getUri(None, None)
      val (extraHeaders, entity): (Map[String, String], HttpEntity) = contentMIMEType match {
        case Some(c) => (Map("Content-Type" -> c), new ByteArrayEntity(contents, ContentType.getByMimeType(c)))
        case _ => (Map(), new ByteArrayEntity(contents))
      }
      putMethod(uri + "/" + id + contentUri,
        queryParams = query(spaceId = spaceId, projectId = projectId, contentFormat = contentFormat),
        entity = Some(entity),
        requestId = requestId,
        expectedStatus = expectedStatus,
        authHeaders = getAuthHeaders,
        extraHeaders = extraHeaders
      )
    }

    def download(id: String,
                 spaceId: Option[String] = None,
                 projectId: Option[String] = None,
                 requestId: Option[String] = None,
                 expectedStatus: Option[Int] = Some(200)): (String, Array[Header], Int) = {
      val uri = getUri(None, None)
      getMethod(
        address = uri + "/" + id + contentUri,
        queryParams = query(
          spaceId = spaceId,
          projectId = projectId
        ),
        expectedStatus = expectedStatus,
        requestId = requestId,
        authHeaders = getAuthHeaders
      )
    }
  }

  import DefaultJsonProtocol._

  object projectsTransactional extends AbstractResource[JsValue, JsValue, JsValue](
    PROJECTS_TRANSACTIONAL,
    js => js,
    js => js,
    entity => entity.toJson,
    classOf[JsValue],
    classOf[JsValue],
    false,
    false,
    false,
    true)

  object projects extends AbstractResource[JsValue, JsValue, JsValue](
    PROJECTS,
    js => js,
    js => js,
    entity => entity.toJson,
    classOf[JsValue],
    classOf[JsValue],
    false)

  import com.ibm.analytics.spaces.api.v2.SpacesJsonFormat.{spaceResourceFormat, spaceResourcesFormat}

  object spacesv2 extends AbstractResource[SpaceEntityRequest, SpaceResource, SpaceResources](
    SPACES,
    js => js.convertTo[SpaceResource],
    js => js.convertTo[SpaceResources],
    entity => entity.toJson,
    classOf[SpaceResource],
    classOf[SpaceResources],
    false,
    false,
    false
  )

  /* OLD /v4/spaces */

  import com.ibm.analytics.wml.api.v4.spaces.SpacesJsonFormat.{SpacesEntityFormat, spacesResourceFormat, spacesResourcesFormat}

  object spaces extends AbstractResource[SpacesEntity, SpacesResource, SpacesResources](
    "/v4/spaces",
    js => js.convertTo[SpacesResource],
    js => js.convertTo[SpacesResources],
    entity => entity.toJson,
    classOf[SpacesResource],
    classOf[SpacesResources],
    false
  )

  object softwareSpecifications extends AbstractResource[SoftwareSpecificationEntity, SoftwareSpecification, SoftwareSpecifications](
    SOFTWARE_SPECIFICATIONS,
    js => js.convertTo[SoftwareSpecification],
    js => js.convertTo[SoftwareSpecifications],
    entity => entity.toJson,
    classOf[SoftwareSpecification],
    classOf[SoftwareSpecifications],
    false
  )

  object hardwareSpecifications extends AbstractResource[HardwareSpecification, HardwareSpecificationResource, HardwareSpecificationResources](
    HARDWARE_SPECIFICATIONS,
    js => js.convertTo[HardwareSpecificationResource],
    js => js.convertTo[HardwareSpecificationResources],
    entity => entity.toJson,
    classOf[HardwareSpecificationResource],
    classOf[HardwareSpecificationResources],
    false
  )

  object dataAssets extends AbstractResource[AssetInput, Asset, Assets](
    DATA_ASSETS,
    js => js.convertTo[Asset],
    js => js.convertTo[Assets],
    entity => entity.toJson,
    classOf[Asset],
    classOf[Assets],
    false
  )

  object camsAssets extends AbstractResourceWithContent[AssetInput, Asset, Assets](
    CAMS_ASSETS,
    CAMS_ASSET_ATTACHMENT,
    js => js.convertTo[Asset],
    js => js.convertTo[Assets],
    entity => entity.toJson,
    classOf[Asset],
    classOf[Assets],
    false
  ) {
    override def upload(id: String,
                        spaceId: Option[String] = None,
                        projectId: Option[String] = None,
                        contents: Array[Byte],
                        contentFormat: Option[String] = Some("native"),
                        contentMIMEType: Option[String] = None,
                        expectedStatus: Option[Int] = Some(204),
                        requestId: Option[String] = None): (String, Array[Header], Int) = {
      val uri = getUri(None, None)
      postMethod(uri + "/" + id + CAMS_ASSET_ATTACHMENT,
        queryParams = query(spaceId = spaceId, projectId = projectId, contentFormat = contentFormat),
        entity = Some(new ByteArrayEntity(contents, ContentType.APPLICATION_JSON)),
        requestId = requestId,
        expectedStatus = expectedStatus,
        authHeaders = getAuthHeaders
      )
    }
  }

}
