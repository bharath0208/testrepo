/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.models

import java.util.Date

import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}

trait ContainsContainer {
  val oldSpaceId: Option[String]
  val oldProjectId: Option[String]
  val newSpaceId: Option[String]
  val newProjectId: Option[String]

  def getNewContainer: Option[Container] = {
    if (newSpaceId.isDefined)
      Some(Space(newSpaceId.get))
    else if (newProjectId.isDefined)
      Some(Project(newProjectId.get))
    else
      None // should never get here
  }

  def getOldContainer: Option[Container] = {
    if (oldSpaceId.isDefined)
      Some(Space(oldSpaceId.get))
    else if (oldProjectId.isDefined)
      Some(Project(oldProjectId.get))
    else
      None // should never get here
  }
}

trait SkipMigratedAssets {
  val skipMigratedAssets: Option[Boolean]

  def shouldSkipMigratedAssets: Boolean = if (skipMigratedAssets.isDefined) skipMigratedAssets.get else true
}

case class MigrationSuccessfulResult(oldAssetType: String,
                                     newAssetType: String,
                                     oldId: String,
                                     newId: String,
                                     assetName: String,
                                     oldModifiedAt: Option[Date] = None)

case class MigrationFailedResult(oldAssetType: String,
                                 oldId: String,
                                 assetName: Option[String] = None,
                                 reason: String,
                                 oldModifiedAt: Option[Date] = None)

case class MigrationResults(successful: Seq[MigrationSuccessfulResult] = Seq.empty,
                            failed: Seq[MigrationFailedResult] = Seq.empty,
                            skipped: Seq[MigrationSuccessfulResult] = Seq.empty)

case class MigrationRequest(
                             oldSpaceId: Option[String] = None,
                             oldProjectId: Option[String] = None,
                             newSpaceId: Option[String] = None,
                             newProjectId: Option[String] = None,
                             experimentIds: Option[Seq[String]] = None,
                             functionIds: Option[Seq[String]] = None,
                             modelIds: Option[Seq[String]] = None,
                             pipelineIds: Option[Seq[String]] = None,
                             modelDefinitionIds: Option[Seq[String]] = None,
                             deploymentJobDefinitionIds: Option[Seq[String]] = None,
                             mapping: Option[Map[String, String]] = None,
                             skipMigratedAssets: Option[Boolean] = None) extends ContainsContainer with SkipMigratedAssets

case class MigrationResource(migrationId: String,
                             status: MigrationStatus,
                             results: Option[MigrationResults] = None,
                             console: Option[Seq[String]] = None,
                             mapping: Option[Map[String, String]] = None,
                             oldSpaceId: Option[String] = None,
                             oldProjectId: Option[String] = None,
                             newSpaceId: Option[String] = None,
                             newProjectId: Option[String] = None,
                             createdAt: Long,
                             modifiedAt: Long) extends ContainsContainer

case class MigrationResources(resources: Seq[MigrationResource] = Seq())

case class MigrationIds(experimentIds: Seq[String] = Seq.empty,
                        functionIds: Seq[String] = Seq.empty,
                        modelIds: Seq[String] = Seq.empty,
                        pipelineIds: Seq[String] = Seq.empty,
                        trainingLibraryIds: Seq[String] = Seq.empty,
                        runtimeIds: Seq[String] = Seq.empty,
                        libraryIds: Seq[String] = Seq.empty,
                        modelDefinitionIds: Seq[String] = Seq.empty,
                        deploymentJobDefinitionIds: Seq[String] = Seq.empty)

case class MigrationDoc(userId: Option[String] = None,
                        status: MigrationStatus,
                        createdAt: Long,
                        modifiedAt: Long,
                        id: Option[String] = None,
                        rev: Option[String] = None,
                        results: Option[MigrationResults] = None,
                        console: Option[Seq[String]] = None,
                        mapping: Option[Map[String, String]] = None,
                        oldSpaceId: Option[String] = None,
                        oldProjectId: Option[String] = None,
                        newSpaceId: Option[String] = None,
                        newProjectId: Option[String] = None,
                        experimentIds: Option[Seq[String]] = None,
                        functionIds: Option[Seq[String]] = None,
                        modelIds: Option[Seq[String]] = None,
                        pipelineIds: Option[Seq[String]] = None,
                        modelDefinitionIds: Option[Seq[String]] = None,
                        deploymentJobDefinitionIds: Option[Seq[String]] = None,
                        skipMigratedAssets: Option[Boolean] = None) extends ContainsContainer with SkipMigratedAssets {
  def addMessageToConsole(msg: String): MigrationDoc = {
    this.copy(console = Some(this.console.map(_ :+ msg).getOrElse(Seq(msg))))
  }

  def addMessagesToConsole(msgs: Seq[String]): MigrationDoc = {
    this.copy(console = Some(this.console.map(_ :++ msgs).getOrElse(msgs)))
  }

  def addSkipped(result: MigrationSuccessfulResult): MigrationDoc = {
    val res: Option[MigrationResults] = results match {
      case Some(res) =>
        Some(res.copy(skipped = res.skipped ++ Seq(result)))
      case None =>
        Some(MigrationResults())
    }
    this.copy(results = res)
  }
}

case class NextJobId(createdAt: Long, id: String)

sealed trait MigrationStatus {
  val name: String
  val isTerminated: Boolean = false

  override def toString: String = name
}

case object Failed extends MigrationStatus {
  override val name = "failed"
  override val isTerminated = true
}

case object Pending extends MigrationStatus {
  override val name = "pending"
}

case object Initializing extends MigrationStatus {
  override val name = "initializing"
}

case object Running extends MigrationStatus {
  override val name = "running"
}

case object Completed extends MigrationStatus {
  override val name = "completed"
  override val isTerminated = true
}

case object Canceled extends MigrationStatus {
  override val name = "canceled"
  override val isTerminated = true
}


// this can be used to flag the asset that was the migration source (can be added to the new asset custom field)

case class MigratedFrom(projectId: Option[String],
                        spaceId: Option[String],
                        assetId: String)


case class AssetInfo(assetId: String, assetType: String, projectId: Option[String], spaceId: Option[String])
