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

import java.util.concurrent.ConcurrentHashMap

import akka.actor.ActorSystem
import akka.dispatch.Dispatcher
import akka.http.scaladsl.model.StatusCodes
import com.typesafe.config.Config
import com.typesafe.scalalogging.StrictLogging

import scala.concurrent.ExecutionContext

// Note that all ActorSystems and ExecutionContexts are controlled centrally in this class from the configuration
object ContextDefinitions extends StrictLogging {
  val SYSTEM_ENDPOINTS = "http-endpoints"
  val SYSTEM_DOWNSTREAM_SERVICES = "downstream-services"
  val SYSTEM_EVENTS = "events"
  val SYSTEM_CAMS_GLOBAL_ASSETS = "cams-global-assets"

  private val actorSystems = new ConcurrentHashMap[String, ActorSystem]()
  private val executionContexts = new ConcurrentHashMap[String, ExecutionContext]()

  private val cd = ContextDefinitions()

  def getDispatcherName(ec: ExecutionContext): String = {
    ec match {
      case dispatcher: Dispatcher =>
        dispatcher.id
      case _ =>
        var name = ec.toString
        if (name.startsWith("Dispatcher[")) {
          name = name.substring(11)
          if (name.endsWith("]"))
            name = name.substring(0, name.length - 1)
        }
        name
    }
  }

  def createActorSystem(config: Config,
                        systemName: String): ActorSystem = {
    val create = new java.util.function.Function[String, ActorSystem]() {
      override def apply(t: String): ActorSystem = {
        systemName match {
          case SYSTEM_DOWNSTREAM_SERVICES =>
            cd.createActorSystem(config = config, actorName = "MLRepositoryDownstreamServices", executionContext = getExecutionContext(systemName))
          case SYSTEM_ENDPOINTS =>
            cd.createActorSystem(config = config, actorName = "MLRepositoryHttpEndpoints", executionContext = getExecutionContext(systemName))
          case SYSTEM_EVENTS =>
            cd.createActorSystem(config = config, actorName = "MLRepositoryEvents", executionContext = getExecutionContext(systemName))
          case SYSTEM_CAMS_GLOBAL_ASSETS =>
            cd.createActorSystem(config = config, actorName = "MLRepositoryCAMSGlobalAssets", executionContext = getExecutionContext(systemName))
          case name: String =>
            throw ServiceException(StatusCodes.InternalServerError, UnrecognizedActorSystemMessage(name))
        }
      }
    }
    actorSystems.computeIfAbsent(systemName, create)
  }

  def getExecutionContext(systemName: String): ExecutionContext = {
    val get = new java.util.function.Function[String, ExecutionContext]() {
      override def apply(t: String): ExecutionContext = {
        val ec = systemName match {
          case SYSTEM_DOWNSTREAM_SERVICES =>
            cd.getExecutionContext(dispatcherName = "outbound-dispatcher", fallbackDispatcherName = "outbound-dispatcher")
          case SYSTEM_ENDPOINTS =>
            cd.getExecutionContext(dispatcherName = "endpoints-dispatcher", fallbackDispatcherName = "endpoints-dispatcher")
          case SYSTEM_EVENTS =>
            cd.getExecutionContext(dispatcherName = "events-dispatcher", fallbackDispatcherName = "outbound-dispatcher")
          case SYSTEM_CAMS_GLOBAL_ASSETS =>
            cd.getExecutionContext(dispatcherName = "global-assets-dispatcher", fallbackDispatcherName = "outbound-dispatcher")
          case name: String =>
            throw ServiceException(StatusCodes.InternalServerError, UnrecognizedActorSystemMessage(name))
        }
        if (ec._1 != getDispatcherName(ec._2))
          throw ServiceException(StatusCodes.InternalServerError, UnrecognizedActorSystemMessage(s"Created execution context ${getDispatcherName(ec._2)} when expecting ${ec._1}"))
        else
          logger.info(s"Created execution context ${ec._2.toString}")
        ec._2
      }
    }
    executionContexts.computeIfAbsent(systemName, get)
  }
}

case class ContextDefinitions() extends StrictLogging {
  private val defaultSystem = ActorSystem()

  def getExecutionContext(dispatcherName: String, fallbackDispatcherName: String): (String, ExecutionContext) = {
    if (defaultSystem.dispatchers.hasDispatcher(dispatcherName))
      (dispatcherName, defaultSystem.dispatchers.lookup(dispatcherName))
    else
      (fallbackDispatcherName, defaultSystem.dispatchers.lookup(fallbackDispatcherName))
  }

  def createActorSystem(config: Config,
                        actorName: String,
                        executionContext: ExecutionContext): ActorSystem = {
    try {
      logger.info(s"Initializing actor system $actorName with dispatcher ${ContextDefinitions.getDispatcherName(executionContext)} ...")
      val sys = ActorSystem.create(
        actorName,
        config,
        classOf[ContextDefinitions].getClassLoader,
        executionContext
      )
      logger.info(s"Initialized actor system ${sys.name} with dispatcher with requested dispatcher ${ContextDefinitions.getDispatcherName(executionContext)}")
      sys
    } catch {
      case e: Exception =>
        logger.error(s"Failed to initialize actor system $actorName (with dispatcher ${ContextDefinitions.getDispatcherName(executionContext)}) : ${e.getMessage}", e)
        throw e
    }
  }
}
