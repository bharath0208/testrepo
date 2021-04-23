/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils.cams

abstract class GlobalAssetTypesException(message: String, cause: Option[Throwable]) extends Exception(message, if (cause.nonEmpty) cause.get else null)

case class CommandLineArgsException(message: String, cause: Option[Throwable] = None) extends GlobalAssetTypesException(message, cause)

case class AuthenticationException(message: String, cause: Option[Throwable] = None) extends GlobalAssetTypesException(message, cause)

case class ExecutionException(message: String, cause: Option[Throwable] = None) extends GlobalAssetTypesException(message, cause)

case class BadResponse(message: String, cause: Option[Throwable] = None) extends GlobalAssetTypesException(message, cause)
