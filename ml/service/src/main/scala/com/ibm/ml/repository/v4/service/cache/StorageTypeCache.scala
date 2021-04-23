/*
 * IBM Confidential
 * OCO Source Materials
 * (C) Copyright IBM Corp. 2017-2021
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.cache

import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.analytics.wml.utils.containers.{Container, ProjectType, SpaceType}
import com.ibm.ml.repository.v4.service.app.ServiceContext
import com.typesafe.scalalogging.StrictLogging

import scala.collection.concurrent.TrieMap
import scala.concurrent.{ExecutionContext, Future}

object StorageTypeCache extends StrictLogging {


    val StorageTypeCache = new TrieMap[String, Future[Option[String]]]()

  def getType(container: Container)
             (implicit sc: ServiceContext, ec: ExecutionContext, identity: Identity): Future[Option[String]] = {
    container.containerType match {
      case SpaceType =>
        // Todo change this once needed
        //for {
        //  space <- sc.containerClient.getSpace(identity, container.id)
        //} yield space.entity.storage.map(_.`type`).getOrElse(AssetFileStorage.name)
        // as long as it is not git type we are good here
        Future.successful(None)
      case ProjectType => for {
        project <- sc.containerClient.getProject(identity, container.id, Map("include"->"everything"))
      } yield Some(project.entity.storage.`type`.name)
        case _ => Future.failed(new NotImplementedError(s"Unknown container type ${container.containerType.name}"))
      }
    }


  def get(container: Container)
         (implicit sc: ServiceContext, ec: ExecutionContext, identity: Identity): Future[Option[String]] = {
    val key = s"${container.id}_${container.containerType.name}"
    StorageTypeCache.get(key) match {
      case Some(result) =>
        logger.info("Get container storage type from cache")
        result
      case None =>
        logger.info("Get container storage type from api")
        getType(container).map { result =>
          StorageTypeCache.addOne(key, Future.successful(result))
          result
          }
      }
    }

}
