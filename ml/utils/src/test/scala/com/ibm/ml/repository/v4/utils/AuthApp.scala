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

import akka.actor.ActorSystem
import com.ibm.analytics.wml.service.utils.security.iam.{IAMContext, IAMStableServiceId}
import com.ibm.analytics.wml.utils.clients.http.{CachedHttpClientBuilder, HttpClientBuilder}
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

object AuthApp extends App with StrictLogging {
  Try {
    implicit val system: ActorSystem = ActorSystem("auth-app")
    implicit val ec: ExecutionContext = system.dispatcher
    implicit val iam: IAMContext = IAMContext()
    implicit val builder: HttpClientBuilder = CachedHttpClientBuilder()

    // args
    val token = Await.result(IAMStableServiceId().createServiceIdToken(), 1.minute)
    logger.info(s"Created service id token $token")
  } match {
    case Success(_) =>
      logger.info("AuthApp finished")
      System.exit(0)
    case Failure(exception) =>
      logger.error(s"AuthApp failed: ${exception.getMessage}", exception)
      System.exit(1)
  }
}
