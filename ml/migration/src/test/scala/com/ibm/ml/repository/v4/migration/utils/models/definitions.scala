/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.utils.models

import java.util.Date

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

case class MigrationOutcome(status: String,
                            results: MigrationResults)