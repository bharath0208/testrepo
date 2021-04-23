/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.upgrade.api.v4

import com.ibm.ngpa.test_automation.framework.SuiteInfo

// this is used to get the suite name
class V4SuiteInfo extends SuiteInfo {
  override def name() = "V4 Repository Migration API Test Suite"
}
