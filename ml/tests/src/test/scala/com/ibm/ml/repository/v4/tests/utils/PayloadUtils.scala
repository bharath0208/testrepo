/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.tests.utils

import scala.language.postfixOps
import scala.util.Try

object PayloadUtils {
  def updateFileResource(resourcePath: String, replaceParams: Option[Map[String, String]] = None): Try[String] = {
    Try {
      val fileRes = getClass.getClassLoader.getResourceAsStream(resourcePath)
      val bArray = LazyList.continually(fileRes.read).takeWhile(-1 !=).map(_.toByte).toArray
      var updatable = new String(bArray, "UTF-8")
      replaceParams.map(_.map(rp => updatable.replaceAll(rp._1, rp._2)))
      updatable
    }
  }
}
