/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.api.v4

import com.ibm.ml.repository.v4.tests.tags.ServiceAPI
import com.ibm.ml.repository.v4.tests.v4.{Container, LoggerSecretsSpec, TidyUpTestsSpec}
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, Suites}

class AllTestsSpec extends Suites with BeforeAndAfterAll with LoggerSecretsSpec {
  override val nestedSuites = {
    IndexedSeq(
      new AllMigrationTestSpec,
      new TidyUpTestsSpec
    )
  }

  override def afterAll(): Unit = {
    checkLogsForSecrets()
  }
}

@ServiceAPI
@DoNotDiscover
class AllMigrationTestSpec extends Suites (
  new AllSpaceMigrationTestsSpec,
  new AllProjectMigrationTestsSpec
)

@DoNotDiscover
@ServiceAPI
class AllSpaceMigrationTestsSpec
  extends ExperimentMigrationTestSpec
    with FunctionMigrationTestSpec
    with ModelMigrationTestSpec
    with PipelineMigrationTestSpec {
  override def containerType: String = "space"

  override def authTokenType: String = "user"

  override def projectId: Option[String] = None

  override def spaceId: Option[String] = Option(Container.getSpace("user")._1)

  override def migrationUserId: String = Container.getSpace("user")._2

  override def deleteContainer: Boolean = false
}

@DoNotDiscover
@ServiceAPI
class AllProjectMigrationTestsSpec
  extends ExperimentMigrationTestSpec
  with FunctionMigrationTestSpec
  with ModelMigrationTestSpec
  with PipelineMigrationTestSpec {
  override def containerType: String = "project"

  override def authTokenType: String = "user"

  override def projectId: Option[String] = Option(Container.getProject("user")._1)

  override def spaceId: Option[String] = None

  override def migrationUserId: String = Container.getProject("user")._2

  override def deleteContainer: Boolean = false
}
