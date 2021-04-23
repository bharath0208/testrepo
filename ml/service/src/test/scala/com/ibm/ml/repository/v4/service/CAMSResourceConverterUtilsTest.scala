/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service

import akka.http.scaladsl.model.Uri
import com.ibm.analytics.cams.api.v2.assets.AssetJsonFormat._
import com.ibm.analytics.cams.api.v2.assets.{AssetMetadata, Assets}
import com.ibm.analytics.wml.api.v4ga.common.HyperReference
import com.ibm.ml.repository.v4.service.utils.CAMSResourceConverterUtils
import com.typesafe.scalalogging.StrictLogging
import org.scalatest.wordspec.AnyWordSpec
import spray.json._
import com.ibm.analytics.wml.utils.containers.{Space, Project}

class CAMSResourceConverterUtilsTest extends AnyWordSpec with StrictLogging with CAMSResourceConverterUtils {

  "convertToWMLMetadata" should {
    "convert space Cams metadata to WML metadata" in {
      val camsMetadata =
        """
          |{
          |        "rov": {
          |            "mode": 0,
          |            "collaborator_ids": {}
          |        },
          |        "space_id": "a4c26493-00f5-40cb-8076-7865f06a97a1",
          |        "usage": {
          |            "last_updated_at": "2020-04-22T14:38:27Z",
          |            "last_updater_id": "1000330999",
          |            "last_update_time": 1587566307542,
          |            "last_accessed_at": "2020-04-22T14:38:27Z",
          |            "last_access_time": 1587566307542,
          |            "last_accessor_id": "1000330999",
          |            "access_count": 0
          |        },
          |        "name": "test_model",
          |        "description": "",
          |        "tags": [],
          |        "asset_type": "wml_model",
          |        "origin_country": "us",
          |        "rating": 0.0,
          |        "total_ratings": 0,
          |        "catalog_id": "dc0f292a-1ba9-4540-a539-acaa23bff8a3",
          |        "created": 1587566307542,
          |        "created_at": "2020-04-22T14:38:27Z",
          |        "owner_id": "1000330999",
          |        "size": 0,
          |        "version": 2.0,
          |        "asset_state": "available",
          |        "asset_attributes": [
          |            "wml_model"
          |        ],
          |        "asset_id": "35419a85-19a3-4ce8-b223-2a160fe578f1",
          |        "asset_category": "USER"
          |}""".stripMargin.parseJson.convertTo[AssetMetadata]

      val wmlMetadata = convertToWMLMetadata(camsMetadata)
      assert(wmlMetadata.id == "35419a85-19a3-4ce8-b223-2a160fe578f1")
      assert(wmlMetadata.createdAt.getTime == 1587566307542L)
      assert(wmlMetadata.modifiedAt.get.getTime == 1587566307542L)
      assert(wmlMetadata.owner.contains("1000330999"))
      assert(wmlMetadata.rev.isEmpty)
      assert(wmlMetadata.name.contains("test_model"))
      assert(wmlMetadata.description.isEmpty)
      assert(wmlMetadata.spaceId.contains("a4c26493-00f5-40cb-8076-7865f06a97a1"))
      assert(wmlMetadata.projectId.isEmpty)
      assert(wmlMetadata.tags.isEmpty)
    }
    "convert project Cams metadata to WML metadata" in {
      val camsMetadata =
        """{
          |                "rov": {
          |                    "mode": 0,
          |                    "collaborator_ids": {}
          |                },
          |                "project_id": "a4c26493-00f5-40cb-8076-7865f06a97a1",
          |                "usage": {
          |                    "last_updated_at": "2020-04-23T17:50:53Z",
          |                    "last_updater_id": "1000330999",
          |                    "last_update_time": 1587664253868,
          |                    "last_accessed_at": "2020-04-23T17:50:53Z",
          |                    "last_access_time": 1587664253868,
          |                    "last_accessor_id": "1000330999",
          |                    "access_count": 0
          |                },
          |                "name": "test_experiment",
          |                "description": "hello",
          |                "tags": ["test"],
          |                "asset_type": "wml_experiment",
          |                "origin_country": "us",
          |                "rating": 0.0,
          |                "total_ratings": 0,
          |                "catalog_id": "dc0f292a-1ba9-4540-a539-acaa23bff8a3",
          |                "created": 1587581386246,
          |                "created_at": "2020-04-22T18:49:46Z",
          |                "owner_id": "1000330999",
          |                "size": 0,
          |                "version": 2.0,
          |                "asset_state": "available",
          |                "asset_attributes": [
          |                    "wml_experiment"
          |                ],
          |                "asset_id": "5ec77ed9-d4dd-4b45-ac85-f9b13896f4b5",
          |                "asset_category": "USER",
          |                "revision_id": 2,
          |                "commit_info": {
          |                    "previous_revision": 1,
          |                    "committed_at": "2020-04-23T17:50:55Z",
          |                    "commit_message": "msg"
          |                }
          |            }
          """.stripMargin.parseJson.convertTo[AssetMetadata]

      val wmlMetadata = convertToWMLMetadata(camsMetadata)
      assert(wmlMetadata.id == "5ec77ed9-d4dd-4b45-ac85-f9b13896f4b5")
      assert(wmlMetadata.createdAt.getTime == 1587581386246L)
      assert(wmlMetadata.modifiedAt.get.getTime == 1587664253868L)
      assert(wmlMetadata.owner.contains("1000330999"))
      assert(wmlMetadata.rev.contains("2"))
      assert(wmlMetadata.name.contains("test_experiment"))
      assert(wmlMetadata.description.contains("hello"))
      assert(wmlMetadata.projectId.contains("a4c26493-00f5-40cb-8076-7865f06a97a1"))
      assert(wmlMetadata.spaceId.isEmpty)
      assert(wmlMetadata.tags.get == Seq("test"))
    }
  }
  "convertToWMLMetadata" should {
    "convert time String with format \"yyyy-MM-dd'T'HH:mm:ss'Z'\" to Date " in {
      val date = convertStringToDate("2020-04-23T17:50:53Z")
      assert(date.getTime == 1587664253000L)
    }

    "convert time String with format \"yyyy-MM-dd'T'HH:mm:ss.SSS'Z'\" to Date " in {
      val date = convertStringToDate("2020-04-23T17:50:53.123Z")
      assert(date.getTime == 1587664253123L)
    }

    "convert time String with wrong format " in {
      val date = convertStringToDate("121421.yutu")
      // it will return current time
      assert(date.getTime > 1587664253123L)
    }
  }
  "getDescription" should {
    "return none when input is empty" in {
      assert(getDescription(Some("")).isEmpty)
    }
    "return none when input is None" in {
      assert(getDescription(None).isEmpty)
    }
    "return 123 when input is 123" in {
      assert(getDescription(Some("123")).contains("123"))
    }
  }
  "getTags" should {
    "return none when input is empty" in {
      assert(getTags(Some(Vector())).isEmpty)
    }
    "return none when input is None" in {
      assert(getTags(None).isEmpty)
    }
    "return 123 when input is 123" in {
      assert(getTags(Some(Vector("123"))).contains(Vector("123")))
    }
  }
  "removeMetadataFieldsFromEntity" should {
    "remove name description spaceId projectId, and tags from entity" in {
      val payload =
        """{
          | "tags" : ["111", "222"],
          | "space_id": "555",
          | "project_id": "666",
          | "name": "test exp",
          | "description": "test des",
          | "label_column" : "test",
          | "custom": {
          |  "test" : "hi"
          | },
          | "evaluation_definition": {
          |      "method": "binary",
          |      "metrics": [
          |        {
          |          "name": "areaUnderROC",
          |          "maximize": true
          |        },
          |        {
          |          "name": "precision",
          |          "maximize": false
          |        },
          |        {
          |          "name": "recall"
          |        }
          |      ]
          |    }
          |
          |}
          |""".stripMargin
      val js = payload.parseJson
      val result = removeMetadataFieldsFromEntity(js).toString()
      assert(!result.contains("tags"))
      assert(!result.contains("space_id"))
      assert(!result.contains("project_id"))
      assert(!result.contains("description"))
      assert(!result.contains("test exp"))
      assert(result.contains("label_column"))
      assert(result.contains("evaluation_definition"))
    }

  }
  "getPageInfo" should {
    "return value" in {
      val assets =
        """{
          |    "next": {
          |        "query": "*:*",
          |        "limit": 2,
          |        "bookmark": "g1AAAABgeJzLYWBgYMxgTmGwTs4vTc5ISXIoTynQhbJ1DfWQeHrluTmFiSZGesVlyXrJOaXFJalFejn5yYk5OUBDmBIZkur___-flcHkZv-5z7kBKJbIlAUA46kgbA",
          |        "include": "entity"
          |    },
          |    "total_rows": 13,
          |    "results": []
          |}
          |""".stripMargin.parseJson.convertTo[Assets]

      val (first, limit, next) =
        getPageInfo(assets, Uri("/ml/v4/wml_experiments?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=1&start=asdfasfsaf"))

      assert(first.contains(HyperReference("/ml/v4/wml_experiments?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=1")))
      assert(limit.contains(2))
      assert(next.contains(HyperReference("/ml/v4/wml_experiments?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=1&start=g1AAAABgeJzLYWBgYMxgTmGwTs4vTc5ISXIoTynQhbJ1DfWQeHrluTmFiSZGesVlyXrJOaXFJalFejn5yYk5OUBDmBIZkur___-flcHkZv-5z7kBKJbIlAUA46kgbA")))
    }
    "return none when it is none" in {
      val assets =
        """{
          |    "total_rows": 13,
          |    "results": []
          |}
          |""".stripMargin.parseJson.convertTo[Assets]

      val (first, limit, next) =
        getPageInfo(assets, Uri("/ml/v4/wml_experiments?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=1&start=asdfasfsaf"))

      assert(first.contains(HyperReference("/ml/v4/wml_experiments?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=1")))
      assert(limit.isEmpty)
      assert(next.isEmpty)
    }
  }

  "getRevisionsPageInfo" should {
    "return value" in {
      val assets =
        """{
          |    "results": []
          |}
          |""".stripMargin.parseJson.convertTo[Assets]

      val (first, next) =
        getRevisionsPageInfo(assets.results, Uri("/ml/v4/wml_experiments/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=2&start=1"), Some(2))

      assert(first.contains(HyperReference("/ml/v4/wml_experiments/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=2")))
      assert(next.isEmpty)
    }
    "return none when it is none" in {
      val assets =
        """{
          |    "results": [
          |        {
          |            "metadata": {
          |                "rov": {
          |                    "mode": 0,
          |                    "collaborator_ids": {}
          |                },
          |                "space_id": "f4c15d2d-f157-4bbf-986b-6a01c174b11b",
          |                "usage": {
          |                    "last_updated_at": "2020-04-24T00:33:30Z",
          |                    "last_updater_id": "1000330999",
          |                    "last_update_time": 1587688410237,
          |                    "last_accessed_at": "2020-04-24T00:33:30Z",
          |                    "last_access_time": 1587688410237,
          |                    "last_accessor_id": "1000330999",
          |                    "access_count": 0
          |                },
          |                "name": "test_model",
          |                "description": "",
          |                "tags": [],
          |                "asset_type": "wml_model",
          |                "origin_country": "us",
          |                "rating": 0.0,
          |                "total_ratings": 0,
          |                "catalog_id": "dd725a36-9f76-4691-8920-9fa2479cadf6",
          |                "created": 1587688387124,
          |                "created_at": "2020-04-24T00:33:07Z",
          |                "owner_id": "1000330999",
          |                "size": 0,
          |                "version": 2.0,
          |                "asset_state": "available",
          |                "asset_attributes": [
          |                    "wml_model"
          |                ],
          |                "asset_id": "b369c823-7929-4923-8555-39f33f1c67c1",
          |                "asset_category": "USER",
          |                "revision_id": 3,
          |                "commit_info": {
          |                    "previous_revision": 2,
          |                    "committed_at": "2020-04-24T00:33:31Z",
          |                    "commit_message": "msg"
          |                }
          |            },
          |            "href": "/v2/assets/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&revision_id=3"
          |        },
          |        {
          |            "metadata": {
          |                "rov": {
          |                    "mode": 0,
          |                    "collaborator_ids": {}
          |                },
          |                "space_id": "f4c15d2d-f157-4bbf-986b-6a01c174b11b",
          |                "usage": {
          |                    "last_updated_at": "2020-04-24T00:33:26Z",
          |                    "last_updater_id": "1000330999",
          |                    "last_update_time": 1587688406513,
          |                    "last_accessed_at": "2020-04-24T00:33:26Z",
          |                    "last_access_time": 1587688406513,
          |                    "last_accessor_id": "1000330999",
          |                    "access_count": 0
          |                },
          |                "name": "test_model",
          |                "description": "",
          |                "tags": [],
          |                "asset_type": "wml_model",
          |                "origin_country": "us",
          |                "rating": 0.0,
          |                "total_ratings": 0,
          |                "catalog_id": "dd725a36-9f76-4691-8920-9fa2479cadf6",
          |                "created": 1587688387124,
          |                "created_at": "2020-04-24T00:33:07Z",
          |                "owner_id": "1000330999",
          |                "size": 0,
          |                "version": 2.0,
          |                "asset_state": "available",
          |                "asset_attributes": [
          |                    "wml_model"
          |                ],
          |                "asset_id": "b369c823-7929-4923-8555-39f33f1c67c1",
          |                "asset_category": "USER",
          |                "revision_id": 2,
          |                "commit_info": {
          |                    "previous_revision": 1,
          |                    "committed_at": "2020-04-24T00:33:29Z",
          |                    "commit_message": "msg"
          |                }
          |            },
          |            "href": "/v2/assets/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&revision_id=2"
          |        }
          |    ]
          |}
          |""".stripMargin.parseJson.convertTo[Assets]

      val (first, next) =
        getRevisionsPageInfo(assets.results, Uri("/ml/v4/wml_experiments/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=2&start=1"), Some(2))

      assert(first.contains(HyperReference("/ml/v4/wml_experiments/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=2")))
      assert(next.contains(HyperReference("/ml/v4/wml_experiments/b369c823-7929-4923-8555-39f33f1c67c1?space_id=f4c15d2d-f157-4bbf-986b-6a01c174b11b&limit=2&start=1")))
    }
  }
  "convertToCAMSMetadata" should {
    "convert" in {
      val assetMetadata = convertToCAMSMetadata(
        name = "test",
        description = Some("des"),
        assetType = "wml_experiment",
        tags = Some(Vector("tag")),
        spaceId = Some("sid"),
        projectId = Some("pid")
      )
      assert(assetMetadata.name == "test")
    }
  }

  "v4ResourcesHref" should {
    "generate path with space" in {
      val url = v4ResourcesHref(
        assetName = "experiment",
        container = Space("dd725a36-9f76-4691-8920-9fa2479cadf6")
      )
      assert(url == "/ml/v4/experiments?space_id=dd725a36-9f76-4691-8920-9fa2479cadf6")
    }
    "generate path with project" in {
      val url = v4ResourcesHref(
        assetName = "experiment",
        container = Project("dd725a36-9f76-4691-8920-9fa2479cadf6")
      )
      assert(url == "/ml/v4/experiments?project_id=dd725a36-9f76-4691-8920-9fa2479cadf6")
    }
    "generate path with limit and start " in {
      val url = v4ResourcesHref(
        assetName = "experiment",
        container = Project("dd725a36-9f76-4691-8920-9fa2479cadf6"),
        limit = Some(100),
        start = Some("aaaaa"),
        tagValue = Some("bbbb")
      )
      assert(url == "/ml/v4/experiments?project_id=dd725a36-9f76-4691-8920-9fa2479cadf6&start=aaaaa&limit=100&tag.value=bbbb")
    }
    "generate path with assetId " in {
      val url = v4ResourcesHref(
        assetName = "experiment",
        container = Project("dd725a36-9f76-4691-8920-9fa2479cadf6"),
        limit = Some(100),
        start = Some("aaaaa"),
        tagValue = Some("bbbb"),
        assetId = Some("cccc")
      )
      assert(url == "/ml/v4/experiments/cccc/revisions?project_id=dd725a36-9f76-4691-8920-9fa2479cadf6&start=aaaaa&limit=100&tag.value=bbbb")
    }
  }
}
