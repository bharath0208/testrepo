/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.service

import com.ibm.ml.repository.v4.tests.tags.{CP4DOnly, CloudOnly, ServiceAPI}
import com.ibm.ml.repository.v4.tests.v4._
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover, Suites}

class AllTestsSpec extends Suites with BeforeAndAfterAll with LoggerSecretsSpec {
  override val nestedSuites = {
    IndexedSeq(
      new CloudAndCpdTests,
      new TidyUpTestsSpec
    )
  }

  override def afterAll(): Unit = {
    checkLogsForSecrets()
  }
}

@ServiceAPI
@DoNotDiscover
class CloudAndCpdTests extends Suites(
  new AllSpaceTestsSpec,
  new AllProjectTestsSpec,
  new CloudPrivateEndpointSpec,
  new CP4DTestSpec
)

@CloudOnly
@ServiceAPI
@DoNotDiscover
class CloudPrivateEndpointSpec extends Suites(
  new SpaceBasicAuthTokenTestsSpec,
  new ProjectBasicAuthTokenTestsSpec
)

@CP4DOnly
@ServiceAPI
@DoNotDiscover
class CP4DTestSpec extends Suites(
  new SpaceBasicAuthTokenTestsSpec,
  new ProjectBasicAuthTokenTestsSpec
)
