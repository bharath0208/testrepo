/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.v4

import com.ibm.ml.repository.v4.tests.tags.{CP4DOnly, CloudOnly, ServiceAPI, TidyUp}
import org.scalatest.{DoNotDiscover, Suites}

// we set these as DoNotDiscover so that we can decide
// which tests to run in the module where the tests
// are actually run - and where we might want to combine
// with other tests...

@ServiceAPI
@DoNotDiscover
class AllSpaceTestsSpec extends Suites (
  new ExperimentsSpecTest("space", "user", Container.getSpace("user"), false),
  new DeploymentJobDefinitionsSpecTest("space", "user", Container.getSpace("user"), false),
  new FunctionsSpecTest("space", "user", Container.getSpace("user"), false),
  new ModelDefinitionsSpecTest("space", "user", Container.getSpace("user"), false),
  new ModelsSpecTest("space", "user", Container.getSpace("user"), false),
  new PipelinesSpecTest("space", "user", Container.getSpace("user"), false),
  new RemoteTrainingSystemsSpecTest("space", "user", Container.getSpace("user"), false),
  new TrainingDefinitionsSpecTest("space", "user", Container.getSpace("user"), false)
)

@ServiceAPI
@DoNotDiscover
class AllProjectTestsSpec extends Suites (
  new ExperimentsSpecTest("project", "user", Container.getProject("user"), false),
  new FunctionsSpecTest("project", "user", Container.getProject("user"), false),
  new ModelDefinitionsSpecTest("project", "user", Container.getProject("user"), false),
  new ModelsSpecTest("project", "user", Container.getProject("user"), false),
  new PipelinesSpecTest("project", "user", Container.getProject("user"), false),
  new RemoteTrainingSystemsSpecTest("project", "user", Container.getProject("user"), false),
  new TrainingDefinitionsSpecTest("project", "user", Container.getProject("user"), false)
)

@CloudOnly
@ServiceAPI
@DoNotDiscover
class SpaceServiceIdTokenTestsSpec extends Suites (
  new FunctionsSpecTest("space", "service-id", Container.getSpace("service-id"), false),
  new ModelsSpecTest("space", "service-id", Container.getSpace("service-id"), false)
)

@CloudOnly
@ServiceAPI
@DoNotDiscover
class ProjectServiceIdTokenTestsSpec extends Suites (
  new FunctionsSpecTest("project", "service-id", Container.getProject("service-id"), false),
  new ModelsSpecTest("project", "service-id", Container.getProject("service-id"), false)
)

@CP4DOnly
@ServiceAPI
@DoNotDiscover
class SpaceBasicAuthTokenTestsSpec extends Suites (
  new FunctionsSpecTest("space", "basic", Container.getSpace("basic"), false),
  new ModelsSpecTest("space", "basic", Container.getSpace("basic"), false)
)

@CP4DOnly
@ServiceAPI
@DoNotDiscover
class ProjectBasicAuthTokenTestsSpec extends Suites (
  new FunctionsSpecTest("project", "basic", Container.getProject("basic"), false),
  new ModelsSpecTest("project", "basic", Container.getProject("basic"), false)
)

@TidyUp
@DoNotDiscover
class TidyUpTestsSpec extends Suites (
  new ContainerCleanupSpec
)
