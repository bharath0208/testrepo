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

import akka.http.scaladsl.model.Uri

import scala.util.{Failure, Success, Try}

object CommandLine {
  val KNOWN_ENVS: Set[String] = Set("ICP", "PROD", "TEST")

  val LIST_ONLY = "--list-only"
  val REQUEST_ID = "--request-id"
  val FORCE_UPDATE = "--force-update"
  val NO_EXIT = "--no-exit"

  def apply(args: Array[String]): CommandLine = {
    if ((args != null) && (args.contains("--usage") || args.contains("--help")))
      CommandLine()
    else {
      val (
        env: String,
        serviceUrl: String,
        authToken: String,
        listOnly: Boolean,
        forceUpdate: Boolean,
        noExit: Boolean,
        requestId: Option[String]
        ) = {
        var listOnly: Boolean = false
        var forceUpdate: Boolean = false
        var requestId: Option[String] = None
        var noExit: Boolean = false
        // arg(0)=env arg(1)=service_url arg(2)=auth/token arg(3+)=options
        if ((args == null) || (args.length < 3)) {
          val msg = "Error: not enough input parameters"
          throw CommandLineArgsException(msg)
        } else {
          for (n <- 0 to 2) {
            val arg = args(n)
            if ((arg == null) || arg.trim.isEmpty) {
              val msg = s"Error: found empty input parameter at position $n"
              throw CommandLineArgsException(msg)
            }
          }
          // check the options
          if (args.length > 3) {
            for (n <- 3 until args.length) {
              val arg = args(n)
              if ((arg == null) || arg.trim.isEmpty) {
                val msg = s"Error: found empty input parameter at position $n"
                throw CommandLineArgsException(msg)
              } else {
                if (LIST_ONLY.equalsIgnoreCase(arg)) {
                  listOnly = true
                } else if (FORCE_UPDATE.equalsIgnoreCase(arg)) {
                  forceUpdate = true
                } else if (NO_EXIT.equalsIgnoreCase(arg)) {
                  noExit = true
                } else if (arg.startsWith(s"$REQUEST_ID=")) {
                  requestId = Some(arg.substring(REQUEST_ID.length + 1))
                } else {
                  val msg = s"Error: found unrecognized parameter $arg at position $n"
                  throw CommandLineArgsException(msg)
                }
              }
            }
          }
        }
        val env = args(0).trim
        if (!KNOWN_ENVS.contains(env.toUpperCase)) {
          val msg = s"""Error: found unrecognized environment $env when expecting one of ${KNOWN_ENVS.mkString(",")}"""
          throw CommandLineArgsException(msg)
        }
        val serviceUrl: String = {
          // for now we do this, this is the old code
          // val surl_items = serviceUrl.split(":")
          // surl_items(1).equals("wmlproxyservice") match {
          //   case true => "https://ibm-nginx-svc/v1/preauth/validateAuth"
          //   case false => serviceUrl + "/v1/preauth/validateAuth"
          // }
          val url = args(1).trim
          val proxyService = "wmlproxyservice"
          val useWmlProxyService = url.contains(proxyService)
          val serviceUrl = if (useWmlProxyService) "https://internal-nginx-svc:12443" else url
          Try(Uri(serviceUrl)) match {
            case Success(uri) =>
              if (uri.authority.isEmpty) {
                val msg = s"Error: no authority in the service url '$serviceUrl'"
                throw CommandLineArgsException(msg)
              }
            case Failure(exception) =>
              val msg = s"Error: not able to parse the service url '$serviceUrl' : ${getMessage(exception)}'"
              throw CommandLineArgsException(msg, Some(exception))
          }
          serviceUrl
        }
        val authToken = args(2).trim

        (env, serviceUrl, authToken, listOnly, forceUpdate, noExit, requestId)
      }
      val isICP: Boolean = "ICP".equalsIgnoreCase(env)
      val isProductionPublicCloud: Boolean = "PROD".equalsIgnoreCase(env)
      CommandLine(
        serviceUrl = serviceUrl,
        auth = authToken,
        listOnly = listOnly,
        forceUpdate = forceUpdate,
        usageOnly = false,
        isICP = isICP,
        isProductionPublicCloud = isProductionPublicCloud,
        noExit = noExit,
        requestId
      )
    }
  }

  def usage(): String = {
    s"""
       |Usage: ${GlobalAssetTypesApp.commandName} "env" "service_url" "authentication" "options"
       |   where "env" is one of:
       |     "ICP"  : when running on CP4D
       |     "PROD" : when running in public cloud
       |     "TEST" : when running in a public cloud test environment
       |   where "service_url" is the URL of the cluster that contains the CAMS service
       |   where "authentication" is one of:
       |     <ICP token>   : provide token when running on CP4D
       |     <IAM API key> : provide IAM API key to create a token when running in cloud
       |   where "options" is optional and can contain 1 or more of the following options:
       |     $LIST_ONLY : just check that the asset types are registered and correct,
       |                   no changes will be made to the CAMS repository
       |     $FORCE_UPDATE : force update of the asset types without checking to see
       |                      if the update is required
       |     --$REQUEST_ID=<request_id> : the request id for HTTP requests
       |     $NO_EXIT : don't exit the JVM when completed or failed
       |     --help | --usage : show this message (can appear anywhere in the arguments)
       |""".stripMargin
  }

  private def getMessage(exception: Throwable): String = {
    if ((exception.getMessage != null) && !exception.getMessage.trim.isEmpty)
      exception.getMessage
    else
      exception.getClass.getName
  }
}

case class CommandLine(serviceUrl: String = "",
                       auth: String = "",
                       listOnly: Boolean = false,
                       forceUpdate: Boolean = false,
                       usageOnly: Boolean = true,
                       isICP: Boolean = false,
                       isProductionPublicCloud: Boolean = false,
                       noExit: Boolean = false,
                       requestId: Option[String] = None) {
  override def toString: String = {
    val sb: StringBuilder = new StringBuilder()

    def addCommand(cmd: String): Unit = {
      if (sb.nonEmpty)
        sb.append(' ')
      sb.append(cmd)
    }

    if (isICP)
      addCommand("ICP")
    else if (isProductionPublicCloud)
      addCommand("PROD")
    else
      addCommand("TEST")
    if ((serviceUrl != null) && !serviceUrl.trim.isEmpty)
      addCommand(serviceUrl)
    if ((auth != null) && !auth.trim.isEmpty)
      addCommand(auth)
    if (listOnly)
      addCommand(CommandLine.LIST_ONLY)
    if (forceUpdate)
      addCommand(CommandLine.FORCE_UPDATE)
    if (noExit)
      addCommand(CommandLine.NO_EXIT)
    if (requestId.isDefined)
      addCommand(s"${CommandLine.REQUEST_ID}=${requestId.get}")

    sb.toString()
  }
}
