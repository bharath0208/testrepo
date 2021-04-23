/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2021
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.utils

import java.time.Duration

import scala.util.{Failure, Success, Try}

object FormatUtils {

  def stripQuotes(s: String): String = {
    if (s.startsWith("\"") && s.endsWith("\""))
      s.substring(1, s.length - 2)
    else
      s
  }

  def formatAsDuration(msecs: Long): String = {
    Try(Duration.ofMillis(msecs).toString) match {
      case Success(value) => value
      case Failure(_) => s"$msecs ms"
    }
  }

}
