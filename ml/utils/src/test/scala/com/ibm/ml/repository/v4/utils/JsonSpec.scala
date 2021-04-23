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

import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionEntity
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.featurespec.AnyFeatureSpec
import spray.json._

import scala.util.{Failure, Success, Try}

class JsonSpec extends AnyFeatureSpec with DefaultJsonProtocol with StrictLogging {
  private case class Data(name: String, id: String)
  private implicit val dataFormat: RootJsonFormat[Data] = jsonFormat2(Data.apply)

  private case class Level(data: Data)
  private implicit val levelFormat: RootJsonFormat[Level] = jsonFormat1(Level.apply)

  private case class Example(data1: Data, data2: Data, level: Option[Level])
  private implicit val exampleFormat: RootJsonFormat[Example] = jsonFormat3(Example.apply)

  private def getMessage(exception: Throwable): String = {
    exception match {
      case e: spray.json.DeserializationException =>
        if (e.fieldNames.nonEmpty) {
          s"${e.getMessage} : required field is ${e.fieldNames.mkString(".")}"
        } else {
          e.getMessage
        }
      case e =>
        e.getMessage
    }
  }

  Feature("JsonParser") {
    Scenario("with custom parser settings") {
      val jsons: Seq[String] = Seq(
        """
          |{
          |  "data1": {
          |    "id": "myid",
          |    "name": "myname"
          |  },
          |  "data2": {
          |    "id": "myid1"
          |  }
          |}
          |""".stripMargin,
        """
          |{
          |  "data1": {
          |    "id": "myid",
          |    "name": "myname"
          |  },
          |  "data2": {
          |    "id": "myid1",
          |    "name": "myname1"
          |  },
          |  "level": {
          |    "data": {
          |      "id": "myid2"
          |    }
          |  }
          |}
          |""".stripMargin
      )

      for (json <- jsons) {
        Try(JsonParser(json)) match {
          case Success(js) =>
            Try(js.convertTo[Example]) match {
              case Success(_) =>
                fail(s"Managed to parse bad json: ${js.prettyPrint}")
              case Failure(exception) =>
                info(s"Parsing failed with error: ${getMessage(exception)}")
            }
          case Failure(exception) =>
            fail(s"Raw json parsing failed with error: ${exception.getMessage}")
        }
      }

      val doModel: String =
        """
          |{
          |  "name": "DO Job for Deployment 7ed52393-5ffe-4e69-9427-bcd5ae75c756",
          |  "deployment": {
          |    "id": "7ed52393-5ffe-4e69-9427-bcd5ae75c756"
          |  },
          |  "decision_optimization": {
          |    "solve_parameters": {
          |      "oaas.parallelAttachmentLimit": "1"
          |    },
          |    "input_data": [
          |      {
          |        "fields": ["name", "unit_cost", "qmin", "qmax"],
          |        "values": [
          |          ["Roasted Chicken", 0.84, 0, 10],
          |          ["Spaghetti W/ Sauce", 0.78, 0, 10],
          |          ["Tomato,Red,Ripe,Raw", 0.27, 0, 10]
          |        ]
          |      }
          |    ],
          |    "output_data": [{
          |      "id": ".*\\.csv"
          |    }]
          |  },
          |  "space_id": "dd1f6e02-e1d3-4ca8-9f7b-bfbcf2dce7f6"
          |}
          |""".stripMargin

      Try(JsonParser(doModel)) match {
        case Success(js) =>
          import com.ibm.analytics.wml.api.v4ga.deployment_job_definitions.DeploymentJobDefinitionJsonFormat._
          Try(js.convertTo[DeploymentJobDefinitionEntity]) match {
            case Success(_) =>
              fail(s"Managed to parse bad json: ${js.prettyPrint}")
            case Failure(exception) =>
              info(s"Parsing failed with error: ${getMessage(exception)}")
          }
        case Failure(exception) =>
          fail(s"Raw json parsing failed with error: ${exception.getMessage}")
      }
    }
  }
}
