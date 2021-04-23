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

import com.ibm.ml.repository.v4.tests.tags.TidyUp
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.{BeforeAndAfterAll, DoNotDiscover}

@TidyUp
@DoNotDiscover
class ContainerCleanupSpec extends AnyFeatureSpec with BeforeAndAfterAll {

  override protected def afterAll(): Unit = {
    super.afterAll()
    Container.deleteProject
    Container.deleteSpace
  }
}
