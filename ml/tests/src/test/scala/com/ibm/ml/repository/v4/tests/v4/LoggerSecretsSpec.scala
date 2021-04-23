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

import java.io.{File, FilenameFilter}
import scala.io.{BufferedSource, Source}
import scala.util.Try

trait LoggerSecretsSpec {
  // "Bearer eyJraWQiOiIyMDIwMDYyNDE4MzAiLCJhbGciOiJSUzI1NiJ9.eyJpYW1faWQiOiJpYW0tU2VydmljZUlkLTgyZTQ0ZTY0LTg4NzgtNGM1Yi1hYzUwLWQ5OGExZGZhNmYwOCIsImlkIjoiaWFtLVNlcnZpY2VJZC04MmU0NGU2NC04ODc4LTRjNWItYWM1MC1kOThhMWRmYTZmMDgiLCJyZWFsbWlkIjoiaWFtIiwiaWRlbnRpZmllciI6IlNlcnZpY2VJZC04MmU0NGU2NC04ODc4LTRjNWItYWM1MC1kOThhMWRmYTZmMDgiLCJuYW1lIjoiV01MLVlQUUEtRU5WLVNUQUJMRS1EQUxMQVMiLCJzdWIiOiJTZXJ2aWNlSWQtODJlNDRlNjQtODg3OC00YzViLWFjNTAtZDk4YTFkZmE2ZjA4Iiwic3ViX3R5cGUiOiJTZXJ2aWNlSWQiLCJhY2NvdW50Ijp7InZhbGlkIjp0cnVlLCJic3MiOiJiMzk5NDA3YjA3ZDU0OTM0YThhMDcyNjZmNWIxOTFkMCJ9LCJpYXQiOjE1OTU0NTcxMjksImV4cCI6MTU5NTQ2MDcyOSwiaXNzIjoiaHR0cHM6Ly9pYW0uY2xvdWQuaWJtLmNvbS9pZGVudGl0eSIsImdyYW50X3R5cGUiOiJ1cm46aWJtOnBhcmFtczpvYXV0aDpncmFudC10eXBlOmFwaWtleSIsInNjb3BlIjoiaWJtIG9wZW5pZCIsImNsaWVudF9pZCI6ImJ4IiwiYWNyIjoxLCJhbXIiOlsicHdkIl19.gayyAoyAlwVa079PXpgLkbTZL-gJdoGFhpBA0HajIXt5PF9N4nDmjfeFWd9D30MSMgzTNrJAw0BJazTLHW_A2DKFC_iGt93qHD5vpXScqpFz85PBn1202thoiu7sbBnP115BlXfez_FHT8QkxqtkWL13RQl1GSNHQFMgNpD6KK66hYewU_tg_W4bXJ9ojQkwCZbZbbabDYbnwi_W-bitDAuNRKb2SrLeB5_0Xo8FizNURkZahwK1-rCTKZLBIUOTAnkAJyEgGhk-SccQZMIAhijlibLMOF6qp7pjrPiYDh6gybYgJAtw79h5xN1tHuTbUhKXSRchuMkCDCjxXYW7tQ"

  def getLogFiles: Array[File] = {
    val dirPath = "target/logs"
    val logDir = new File(dirPath)
    assert(logDir.exists(), s"Failed to find the logs directory '$dirPath'")
    logDir.listFiles(
      new FilenameFilter() {
        override def accept(dir: File, name: String): Boolean = {
          name.endsWith(".log")
        }
      }
    )
  }

  def checkLogsForSecrets(): Unit = {
    var secrets = 0
    for (log <- getLogFiles) {
      val file = log.getCanonicalPath
      val (found, lines) = checkFileForSecrets(file)
      secrets += found
      println(s"Checked log file $file for secrets and found $found in $lines lines")
    }
    println(s"found $secrets secrets in log files")
    assert(secrets == 0, s"found $secrets secrets in log files")
  }

  /*
  def checkFileForSecrets(file: String): (Int, Int) = {
    var secrets = 0
    var lineNumber = 1
    Using ( Source.fromFile(file)) { source =>
      for (line <- source.getLines()) {
        if (foundSecret(line, lineNumber, file))
          secrets += 1
        lineNumber += 1
      }
    }
    (secrets, lineNumber)
  }
   */

  def checkFileForSecrets(file: String): (Int, Int) = {
    var secrets = 0
    var lineNumber = 1
    var source: BufferedSource = null
    try {
      source = Source.fromFile(file)
      for (line <- source.getLines()) {
        if (foundSecret(line, lineNumber, file))
          secrets += 1
        lineNumber += 1
      }
    }
    finally {
      Try(source.close())
    }
    (secrets, lineNumber)
  }

  private val bearerTokensRE = "Bearer[ ]+(ey[a-zA-Z0-9._-]{20,})".r
  private val passwordRE = "([Pp][Aa][Ss][Ss][Ww][Oo][Rr][Dd][ ]+.+$)".r
  private val emailRE = "([a-zA-Z0-9.-]+[@]([a-zA-Z0-9]+\\.){1,2}[a-zA-Z0-9]+)".r

  def foundSecret(line: String, lineNumber: Int, file: String): Boolean = {
    line match {
      case bearerTokensRE(token) =>
        println(s"Found token '$token' in file $file at line $lineNumber")
        true
      case passwordRE(password) =>
        println(s"Found password '$password' in file $file at line $lineNumber")
        true
      case emailRE(email) =>
        println(s"Found email '$email' in file $file at line $lineNumber")
        true
      case _ =>
        false
    }
  }
}
