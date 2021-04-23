/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.utils

import akka.http.scaladsl.client.RequestBuilding
import akka.http.scaladsl.model.headers.RawHeader
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec
import spray.json._

import scala.concurrent.duration._
import scala.util.Try

class MLSpec extends AnyWordSpec with StrictLogging {
  "ML" when {
    "checking ML utilities" should {
      "have deployment platform" in {
        val isPublic = isPublicCloud
        val isPrivate = isPrivateCloud
        val isDev = isDevelopment
        if (isPublic) {
          assert(!isPrivate)
          assert(!isDev)
        } else if (isPrivate) {
          assert(!isPublic)
          assert(!isDev)
        } else if (isDev) {
          assert(!isPublic)
          assert(!isPrivate)
        } else {
          fail("No environment is set")
        }
      }
      "have WML auth" in {
        val auth: AuthType = getWmlAuth
        assert(auth != null)
        info(s"Auth: ${AUTH_NONE.name}")
        info(s"Auth: ${AUTH_ICP.name}")
        info(s"Auth: ${AUTH_IAM.name}")
      }
      "have HTTP header names" in {
        assert(IP_HEADERS.nonEmpty)
        assert(DOWNSTREAM_HEADERS.nonEmpty)
        assert(COPY_HEADERS.nonEmpty)
        assert(SECURITY_HEADERS.nonEmpty)
        assert(NO_CACHE_HEADERS.nonEmpty)
      }
      "log json" in {
        val json = JsObject("name" -> JsString("Julian"), "address" -> JsString("Nice"), "token" -> JsString("ahsigewfiuhfdsuhvhu"))
        info(logPrint(js = json, masked = Some(true), prettyPrint = Some(true)))
        info(logPrint(js = json, masked = Some(true), prettyPrint = Some(false)))
        info(logPrint(js = json, masked = Some(false), prettyPrint = Some(true)))
        info(logPrint(js = json, masked = Some(false), prettyPrint = Some(false)))
        info(maskedPrint(js = json, prettyPrint = Some(false)))
      }
      "mask tokens" in {
        val tokens = Seq(
          "iUhVS46FsHwV_NQ8cZ_BCi7xgSRSmtgDWwGx53qBKxfVmul_CjTLned9mFXFvfLiVXTmPnFtN9YUwM2CCubyAaJ0OQ7CBRjmTtc7ps9AZsGarUMV3DkXnHvi7QeWw",
          "",
          "1",
          "12",
          "123",
          "1234",
          "12345",
          "123456"
        )
        for (token <- tokens) {
          info(s"Masked token: ${maskToken(token)}")
        }
      }
      "format dates" in {
        val now: Long = System.currentTimeMillis()
        val date: String = formatAsDate(now)
        assert((date != null) && date.trim.nonEmpty)
        info(s"UTC date is $date")
        val dur1: Long = (1000 * 44) + 320
        val duration1: String = formatAsDuration(dur1)
        assert((duration1 != null) && duration1.trim.nonEmpty)
        info(s"Duration is $duration1")
        assert(duration1.contains("32"))
        assert(duration1.contains("44"))
        val dur2: Long = 320
        val duration2: String = formatAsDuration(dur2)
        assert((duration2 != null) && duration2.trim.nonEmpty)
        info(s"Duration is $duration2")
        assert(duration2.contains("32"))
      }
      "format dates compatible" in {
        val dates: Seq[Long] = Seq(System.currentTimeMillis())
        for (date <- dates) {
          val msecsDate = formatAsDate(date)
          val newDate = formatAsDateNew(date)
          // this is missing the msecs as .001 etc
          assert(newDate.startsWith(msecsDate.substring(0, msecsDate.length - 1)))
        }
      }
      "get durations" in {
        val dur1 = getDuration("tests.duration")
        assert(dur1.toMinutes == 10)
        val dur2 = Try(getDuration("tests.duration-does-not-exist"))
        assert(dur2.isFailure)
        val dur3 = getDuration("tests.duration", 5.minutes)
        assert(dur3.toMinutes == 10)
        val dur4 = getDuration("tests.duration-does-not-exist", 5.minutes)
        assert(dur4.toMinutes == 5)
      }
      "show version dates" in {
        info(s"GA version date: ${V4RepositoryVersions.V4_REPOSITORY_GA}")
        val date = Try(V4RepositoryVersions.VD("199-199-19"))
        assert(date.isFailure)
        assert(date.failed.get.isInstanceOf[ServiceException])
      }
      "handle HTTP requests" in {
        val request1 = RequestBuilding.Get("/ml/v4/experiments")
        assert(getRequestId(request1).isEmpty)

        val request2a = RequestBuilding.Get("/ml/v4/experiments").addHeader(RawHeader(HEADER_REQUEST_ID, "julian1"))
        val reqId2a: Option[String] = getRequestId(request2a)
        assert(reqId2a.nonEmpty)
        assert(reqId2a.get == "julian1")

        val request2b = RequestBuilding.Get("/ml/v4/experiments").addHeader(RawHeader(HEADER_GLOBAL_TRANSACTION_ID, "julian2"))
        val reqId2b: Option[String] = getRequestId(request2b)
        assert(reqId2b.nonEmpty)
        assert(reqId2b.get == "julian2")

        val request3 = RequestBuilding.Get("/ml/v4/experiments").addHeader(RawHeader("X-Forwarded-For", "julian3"))
        val headers: Map[String, String] = downstreamServiceHeaders(request3)
        assert(headers.size == 1)
        val value: Option[String] = headers.get("X-Forwarded-For")
        assert(value.nonEmpty)
        assert(value.get == "julian3")
      }
      "check date" in {
        val date: Long = config.getLong("service.v1-lite-plans.start-checks")
        assert (date < Long.MaxValue)
      }
    }
  }
}
