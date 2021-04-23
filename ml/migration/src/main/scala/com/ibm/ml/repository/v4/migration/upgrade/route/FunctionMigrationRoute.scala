/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.route

import akka.actor.ActorSystem
import com.ibm.analytics.wml.api._
import com.ibm.analytics.wml.api.v4.functions.FunctionJsonFormat._
import com.ibm.analytics.wml.api.v4.functions.FunctionResource
import com.ibm.analytics.wml.api.v4ga.functions.FunctionEntityRequest
import com.ibm.analytics.wml.service.utils.security.model.Identity
import com.ibm.ml.repository.v4.migration.upgrade.converters.FunctionConverter
import com.ibm.ml.repository.v4.migration.upgrade.job.MigrationJob
import com.ibm.ml.repository.v4.migration.upgrade.models.{MigrationDoc, MigrationIds, MigrationResults}
import spray.json._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

case class FunctionMigrationRoute(mj: MigrationJob)(implicit system: ActorSystem, identity: Identity)
  extends AbstractMigrationRoute[FunctionResource, FunctionEntityRequest](mj)(
    system,
    identity,
    v4.functions.FunctionJsonFormat.functionResourceFormat,
    v4ga.functions.FunctionJsonFormat.functionEntityRequestFormat
  ) with FunctionConverter {

  override def oldAssetType: String = FUNCTIONS_TYPE

  override def newAssetType: String = FUNCTIONS_TYPE

  override protected def getOldAssetAction(id: String, doc: MigrationDoc): Future[(FunctionResource, Option[String], Seq[String])] = {
    for {
      oldFunction <- mj.sc.v4BetaRepositoryClient.functions.get(identity = identity, id = id, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
    } yield (oldFunction, None, Seq.empty)
  }

  override protected def getDependenciesAction(oldResource: FunctionResource, doc: MigrationDoc): Future[MigrationIds] = {
    val ids = oldResource.entity.runtime.map { runtime =>
      MigrationIds(runtimeIds = Seq(runtime.getId()))
    }.getOrElse(MigrationIds())

    Future.successful(ids)
  }

  override protected def convertToRequestAction(oldResource: FunctionResource, dependencies: MigrationIds, doc: MigrationDoc): Future[(FunctionEntityRequest, Seq[String])] = {
    val entity = oldResource.entity
    val metadata = oldResource.metadata

    val oldEntity = entity.toJson.asJsObject.fields - FIELD_TAGS - FIELD_SPACE - FIELD_NAME -
      FIELD_PROJECT - FIELD_SPACE_ID - FIELD_PROJECT_ID - FIELD_RUNTIME

    val newName = getNewName(entity.name, oldAssetType, metadata.id)
    val newIds = addSpaceOrProjectId(doc.newSpaceId, doc.newProjectId)
    val newTags = convertTags(entity.tags)

    // ui is using this as the default software spec name
    val defaultSoftwareSpec = entity.softwareSpec.map(spec => upgradeSoftwareSpec(mj.softwareSpecs, spec.id, spec.name))
      .getOrElse(getBaseSoftwareSpecByName(mj.softwareSpecs, DEFAULT_FUNCTION_SOFTWARE_SPEC))
    val (softwareSpecRef, consoleMessages) = Try {
      convertResourceRef(
        entity.runtime,
        RUNTIMES_TYPE,
        FIELD_SOFTWARE_SPEC,
        doc.results.getOrElse(MigrationResults()),
        doc.mapping,
        defaultSoftwareSpec
      )
    } match {
      case Success(value) => (value, Seq.empty)
      case Failure(exception) =>
        val msg = s"Unable to convert the runtime for function asset ${metadata.id} due to ${exception.getMessage} to a software specification. Using default software spec $DEFAULT_FUNCTION_SOFTWARE_SPEC instead"
        logger.error(msg)
        val consoleMsgs = Seq(s"Warning: $msg. If the deployment fails, create the asset manually.")
        (defaultSoftwareSpec, consoleMsgs)
    }


    import com.ibm.analytics.wml.api.v4ga.functions.FunctionJsonFormat._
    val newRequest = (oldEntity ++ newName ++ newIds ++ newTags ++ softwareSpecRef).toJson.convertTo[FunctionEntityRequest]
    Future.successful(newRequest, consoleMessages)
  }

  override protected def createNewAssetAction(request: FunctionEntityRequest, oldResource: FunctionResource, doc: MigrationDoc): Future[String] = {
    val oldId = oldResource.metadata.id
    val projectId = doc.newProjectId
    val spaceId = doc.newSpaceId
    for {
      (resource, _) <- mj.sc.mlRepositoryClient.functions.create(
        identity = identity,
        version = ML_REPO_VERSION,
        entity = request.copy(custom = addMigratedFrom(request.custom, oldResource.metadata.id, doc))
      )
      _ <- {
        (for {
          source <- mj.sc.v4BetaRepositoryClient.functions.download(identity, oldId, spaceId = doc.oldSpaceId, projectId = doc.oldProjectId)
          _ <- mj.sc.mlRepositoryClient.functions.upload(identity,
            ML_REPO_VERSION,
            resource.metadata.id,
            content = source,
            projectId = projectId,
            spaceId = spaceId
          )
        } yield {

        }) recoverWith {
          case e: Throwable =>
            // if the function does not contains content, we will ignore the error
            val msg = s"Skip to migrate the content for the function asset $oldId: ${e.getMessage}"
            logger.info(msg)
            Future.successful(())
        }
      }
    } yield resource.metadata.id
  }

  override def getNewAsset(newId: String, doc: MigrationDoc): Future[Any] = {
    mj.sc.mlRepositoryClient.functions.get(
      identity = identity,
      version = ML_REPO_VERSION,
      id = newId,
      spaceId = doc.newSpaceId,
      projectId = doc.newProjectId)
  }

}
