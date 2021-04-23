/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils.v4beta.repository

import java.util.concurrent.{ConcurrentHashMap, ConcurrentMap}

import akka.actor.ActorSystem
import akka.http.scaladsl.model.headers.{Authorization, OAuth2BearerToken}
import com.ibm.analytics.wml.service.utils.http.{ServiceAuth, WMLUserIdHeader}
import com.ibm.analytics.wml.service.utils.security.iam.{IAM, IAMContext, IAMStableServiceId}
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.clients.http.HttpClientBuilder
import com.ibm.analytics.wml.utils.security._
import com.ibm.ml.repository.v4.migration.models.OldInstance
import com.ibm.ml.repository.v4.utils.ContextUtils
import com.ibm.ml.repository.v4.utils.logging.reqId
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Success, Try}

object CachedCredentialsProvider extends ContextUtils with StrictLogging {
  private val TOKEN_CACHE_PERIOD: Duration = 30.minutes
  private val tokenCache: ConcurrentMap[String, (String, Long)] = new ConcurrentHashMap[String, (String, Long)]()

  def getTokenFromApiKey(oldInstance: OldInstance)
                        (implicit system: ActorSystem,
                         builder: HttpClientBuilder,
                         iamContext: IAMContext): Future[String] = {
    implicit val ec: ExecutionContext = system.dispatcher

    Try(tokenCache.get(oldInstance.apiKey.get)) match {
      case Success((token, date)) if System.currentTimeMillis() < (date + TOKEN_CACHE_PERIOD.toMillis) =>
        Future.successful(token)
      case _ =>
        for {
          token <- IAM.getFromApiKey(oldInstance.apiKey.get)
        } yield {
          tokenCache.put(oldInstance.apiKey.get, (token, System.currentTimeMillis()))
          token
        }
    }
  }

  def getServiceIdToken()
                       (implicit system: ActorSystem,
                        builder: HttpClientBuilder,
                        iamContext: IAMContext): Future[String] = {
    implicit val ec: ExecutionContext = system.dispatcher

    val apiKey = "__service_id__"

    Try(tokenCache.get(apiKey)) match {
      case Success((token, date)) if System.currentTimeMillis() < (date + TOKEN_CACHE_PERIOD.toMillis) =>
        Future.successful(token)
      case _ =>
        for {
          token <- IAMStableServiceId().createServiceIdToken()
        } yield {
          tokenCache.put(apiKey, (token, System.currentTimeMillis()))
          token
        }
    }
  }

  def getCredentials(identity: Identity,
                     oldInstance: OldInstance)
                    (implicit system: ActorSystem,
                     builder: HttpClientBuilder,
                     iamContext: IAMContext): Future[Map[String, String]] = {
    implicit val ec: ExecutionContext = system.dispatcher

    if (oldInstance.apiKey.isDefined) {
      for {
        token <- getTokenFromApiKey(oldInstance)
      } yield {
        val ob = OAuth2BearerToken(token)
        Map(
          Authorization.name -> ob.value,
          MLInstanceIdHeader.name -> oldInstance.instanceId
        )
      }
    } else {
      // when we get here we assume that this is an identity from a user token
      if (ServiceAuth.isAllowedServiceId(identity)) {
        // Subject.toString handles printing of masked fields
        reqId(identity.requestId)(() => logger.warn(s"Handling call to migration API with WML service id ${identity.subject}"))
      }
      for {
        token <- getServiceIdToken()
      } yield {
        val ob = OAuth2BearerToken(token)
        Map(
          Authorization.name -> ob.value,
          MLInstanceIdHeader.name -> oldInstance.instanceId,
          WMLUserIdHeader.name -> identity.subject.id
        )
      }
    }
  }
}
