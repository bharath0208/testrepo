/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.service.utils

import akka.http.scaladsl.model.StatusCodes
import com.ibm.analytics.cams.api.v2.assets.{Asset, AssetAttachmentMetadata}
import com.ibm.analytics.projects.api.v2.LocalGitStorage
import com.ibm.analytics.wml.api.v4ga.common.CommonJsonFormat._
import com.ibm.analytics.wml.api.v4ga.common._
import com.ibm.analytics.wml.api.v4ga.trainings.fl.FederatedLearning
import com.ibm.analytics.wml.cams.CAMSClient
import com.ibm.analytics.wml.environments.EnvironmentsClient
import com.ibm.analytics.wml.utils.containers.{Container, Project, Space}
import com.ibm.analytics.wml.utils.specs.{HardwareSpec, SoftwareSpec}
import com.ibm.ml.repository.v4.service.utils.baseOrDerivedType.baseOrDerivedType
import com.ibm.ml.repository.v4.utils.{InvalidRequestEntityMessage, MissingOneOfQueryParametersMessage, ServiceException, _}
import com.typesafe.scalalogging.StrictLogging
import spray.json.{JsArray, JsObject, JsString, JsValue}

import scala.collection.immutable.Map
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

trait ValidateUtils extends StrictLogging {

  def toSoftwareSpec(swSpec: Option[SoftwareSpecRef]): Option[SoftwareSpec] = {
    swSpec.map { spec =>
      SoftwareSpec(id = spec.id, rev = spec.rev, name = spec.name, baseId = spec.baseId)
    }
  }

  def toHardwareSpec(hdSpec: Option[HardwareSpecRef]): Option[HardwareSpec] = {
    hdSpec.map { spec =>
      HardwareSpec(id = spec.id, rev = spec.rev, name = spec.name, numNodes = spec.numNodes, baseId = spec.baseId)
    }
  }

  private def container(spaceId: Option[String],
                        projectId: Option[String]): Container = {
    if (spaceId.isDefined)
      Space(spaceId.get)
    else if (projectId.isDefined)
      Project(projectId.get)
    else {
      // should never get here
      throw ServiceException(StatusCodes.BadRequest, MissingOneOfQueryParametersMessage("space_id", "project_id"))
    }
  }

  def unsafeCharactersValidator(fieldValue: Option[String], fieldName: String, blankCheck: Boolean = false): Option[String] = {
    fieldValue.map(unsafeCharactersValidator(_, fieldName, blankCheck))
  }

  def unsafeCharactersValidator(fieldValue: String, fieldName: String, blankCheck: Boolean): String = {
    val disallowedCharacters = Seq("%", """\""", "<", ">")
    if (blankCheck) {
      stringFieldValidator(fieldValue, fieldName)
    }
    disallowedCharacters.foreach(char => {
      if (fieldValue.contains(char))
        invalidCharactersError(fieldName, disallowedCharacters)
    })
    fieldValue.trim
  }

  def stringFieldValidator(fieldValue: Option[String], fieldName: String): Option[String] = {
    fieldValue.map(stringFieldValidator(_, fieldName))
  }

  def stringFieldValidator(fieldValue: String, fieldName: String): String = {
    if (fieldValue.trim.isEmpty) {
      blankError(s"$fieldName field")
    }
    fieldValue.trim
  }

  def arrayOfStringFieldValidator(fieldValue: Seq[String], fieldName: String): Seq[String] = {
    fieldValue.map { v =>
      if (v.trim.isEmpty) blankError(fieldName)
      v.trim
    }
  }

  def blankError(fieldName: String): Nothing = {
    // todo we need move all the error message to ServiceException
    val msg = s"$fieldName cannot be blank."
    throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
  }


  def invalidCharactersError(fieldName: String, disallowedCharacters: Seq[String]): Nothing = {
    // todo we need move all the error message to ServiceException
    val msg = s"""$fieldName cannot contain characters [${disallowedCharacters.mkString(" ")}]"""
    throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
  }

  def emptyArrayError(fieldName: String): Nothing = {
    val msg = s"$fieldName can not be an empty array."
    throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
  }

  def contentTypeNotAllowedError(contentType: String, supported: String): Nothing = {
    throw ServiceException(StatusCodes.BadRequest, ContentTypeNotAllowedMessage(contentType, supported))
  }

  def contentTypeNotAllowedForModelTypeError(contentType: String, modelType: String, supported: String): Nothing = {
    throw ServiceException(StatusCodes.BadRequest, ContentTypeNotAllowedForModelTypeMessage(contentType, modelType, supported))
  }

  def emptyArrayValidator(fieldName: String, array: Vector[Any]): Unit = {
    if (array.isEmpty) {
      emptyArrayError(fieldName)
    }
  }

  def emptyArrayValidator(fieldName: String, array: Option[Vector[Any]]): Unit = {
    array.foreach(a => {
      emptyArrayValidator(fieldName, a)
    })
  }

  def entitySpaceIdOrProjectIdValidator(spaceId: Option[String],
                                        projectId: Option[String]): (Option[String], Option[String]) = {
    val applySpaceId = spaceId.map(_.trim)
    val applyProjectId = projectId.map(_.trim)
    if (
      ((applySpaceId.isEmpty || applySpaceId.get == "") && (applyProjectId.isEmpty || applyProjectId.get == "")) ||
        (applySpaceId.isDefined && applyProjectId.isDefined)
    ) {
      val msg = "Either space_id or project_id has to be provided."
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
    (applySpaceId, applyProjectId)
  }

  private def getUserFS(implicit callContext: CallContext): Option[Boolean] = {
    callContext.containerStorageType.map {
      case LocalGitStorage.name => true
      case _ => false
    }
  }

  protected def getHardwareSpecHelperMaps(envClient: EnvironmentsClient)
                                         (implicit callContext: CallContext,
                                          ec: ExecutionContext):
  Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] = {

    for {
      hardwareResources <- envClient.getHardwareSpecsResources(callContext.identity, None, Some(callContext.container), getUserFS)
    } yield {
      val NameToIdMap: mutable.Map[String, Vector[(String, baseOrDerivedType)]] = mutable.Map()
      val IdToTypeMap: mutable.Map[String, (String, baseOrDerivedType)] = mutable.Map()
      hardwareResources.resources.foreach(resource => {
        val name = resource.metadata.name.getOrElse("")
        val assetId = resource.metadata.assetId
        val baseOrDerived = if (resource.metadata.spaceId.isDefined ||
          resource.metadata.projectId.isDefined) baseOrDerivedType.DERIVED else baseOrDerivedType.BASE
        if (NameToIdMap contains name) {
          NameToIdMap(name) = NameToIdMap(name) :+ (assetId, baseOrDerived)
        } else {
          NameToIdMap(name) = Vector((assetId, baseOrDerived))
        }
        IdToTypeMap(assetId) = (name, baseOrDerived)
      })
      (NameToIdMap.toMap, IdToTypeMap.toMap)
    }
  }

  protected def getSoftwareSpecHelperMaps(envClient: EnvironmentsClient)
                                         (implicit callContext: CallContext,
                                          ec: ExecutionContext):
  Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] = {
    for {
      softwareResources <- envClient.getSoftwareSpecsResources(callContext.identity, None, Some(callContext.container), getUserFS)
    } yield {
      val NameToIdMap: mutable.Map[String, Vector[(String, baseOrDerivedType)]] = mutable.Map()
      val IdToTypeMap: mutable.Map[String, (String, baseOrDerivedType)] = mutable.Map()
      softwareResources.resources.foreach(resource => {
        val name = resource.metadata.name.getOrElse("")
        val assetId = resource.metadata.assetId
        val baseOrDerived = resource.entity.software_specification.sType match {
          case Some(sType) =>
            sType match {
              case "derived" => baseOrDerivedType.DERIVED
              case "base" => baseOrDerivedType.BASE
              case _ => if (resource.metadata.spaceId.isDefined ||
                resource.metadata.projectId.isDefined) baseOrDerivedType.DERIVED else baseOrDerivedType.BASE
            }
          case None =>
            if (resource.metadata.spaceId.isDefined ||
              resource.metadata.projectId.isDefined) baseOrDerivedType.DERIVED else baseOrDerivedType.BASE
        }
        if (NameToIdMap contains name) {
          NameToIdMap(name) = NameToIdMap(name) :+ (assetId, baseOrDerived)
        } else {
          NameToIdMap(name) = Vector((assetId, baseOrDerived))
        }
        IdToTypeMap(assetId) = (name, baseOrDerived)
      })
      (NameToIdMap.toMap, IdToTypeMap.toMap)
    }
  }

  protected def validateHardwareSpecs(hardwareSpecRef: HardwareSpecRef,
                                      hardwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                      hardwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)]): HardwareSpecRef = {
    if (hardwareSpecRef.id.isEmpty && hardwareSpecRef.baseId.isEmpty) {
      val name = hardwareSpecRef.name.get
      if (!hardwareSpecNameToIdMap.contains(name)) {
        val msg = s"No matching name found for hardware specification $name."
        throw ServiceException(StatusCodes.NotFound, InvalidRequestEntityMessage(msg))
      } else if (hardwareSpecNameToIdMap(name).length > 1) {
        val msg = s"More than one matching name found for hardware specification $name."
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      } else {
        val (assetId, baseOrDerived) = hardwareSpecNameToIdMap(name)(0)
        baseOrDerived match {
          case baseOrDerivedType.BASE =>
            hardwareSpecRef.copy(baseId = Some(assetId))
          case baseOrDerivedType.DERIVED =>
            hardwareSpecRef.copy(id = Some(assetId))
        }
      }
    } else if (hardwareSpecRef.id.isDefined) { // convert to baseId if is base
      val assetId = hardwareSpecRef.id.get
      val (specName, baseOrDerived) = hardwareSpecIdToTypeMap.getOrElse(assetId, ("", None))
      baseOrDerived match {
        case baseOrDerivedType.BASE =>
          hardwareSpecRef.copy(baseId = Some(assetId), id = None, name = Some(specName))
        case baseOrDerivedType.DERIVED =>
          hardwareSpecRef.copy(id = Some(assetId), baseId = None, name = Some(specName))
        case None =>
          val msg = s"Hardware specification id `$assetId` does not exist."
          throw ServiceException(StatusCodes.NotFound, InvalidRequestEntityMessage(msg))
      }
    } else {
      hardwareSpecRef
    }
  }

  protected def validateSoftwareSpecs(softwareSpecRef: SoftwareSpecRef,
                                      softwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                      softwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)]): SoftwareSpecRef = {
    if (softwareSpecRef.id.isEmpty && softwareSpecRef.baseId.isEmpty) {
      val name = softwareSpecRef.name.get
      if (!softwareSpecNameToIdMap.contains(name)) {
        val msg = s"No matching name found for software specification $name."
        throw ServiceException(StatusCodes.NotFound, InvalidRequestEntityMessage(msg))
      } else if (softwareSpecNameToIdMap(name).length > 1) {
        val msg = s"More than one matching name found for software specification $name."
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      } else {
        val (assetId, baseOrDerived) = softwareSpecNameToIdMap(name)(0)
        baseOrDerived match {
          case baseOrDerivedType.BASE =>
            softwareSpecRef.copy(baseId = Some(assetId))
          case baseOrDerivedType.DERIVED =>
            softwareSpecRef.copy(id = Some(assetId))
        }
      }
    } else if (softwareSpecRef.id.isDefined) { // convert to baseId if is base
      val assetId = softwareSpecRef.id.get
      val (specName, baseOrDerived) = softwareSpecIdToTypeMap.getOrElse(assetId, ("", None))
      baseOrDerived match {
        case baseOrDerivedType.BASE =>
          softwareSpecRef.copy(baseId = Some(assetId), id = None, name = Some(specName))
        case baseOrDerivedType.DERIVED =>
          softwareSpecRef.copy(id = Some(assetId), baseId = None, name = Some(specName))
        case None =>
          logger.info(s"Software specification $assetId does not exist in $softwareSpecNameToIdMap and $softwareSpecIdToTypeMap")
          val msg = s"Software specification id `$assetId` does not exist."
          throw ServiceException(StatusCodes.NotFound, InvalidRequestEntityMessage(msg))
      }
    } else {
      softwareSpecRef
    }
  }

  //softwareSpec is NOT optional
  def softwareSpecValidator(softwareSpec: SoftwareSpecRef,
                            envClient: EnvironmentsClient)
                           (implicit callContext: CallContext, ec: ExecutionContext): Future[SoftwareSpecRef] = {
    for {
      (softwareSpecNameToIdMap, softwareSpecIdToTypeMap) <- getSoftwareSpecHelperMaps(envClient)
    } yield {
      validateSoftwareSpecs(softwareSpec, softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
    }
  }

  def softwareSpecValidator(softwareSpec: Option[SoftwareSpecRef],
                            envClient: EnvironmentsClient)
                           (implicit callContext: CallContext, ec: ExecutionContext): Future[Option[SoftwareSpecRef]] = {
    // if softwareSpec exist
    if (softwareSpec.isDefined) {
      for {
        softwareSpecRef <- softwareSpecValidator(softwareSpec.get, envClient)
      } yield {
        softwareSpec.map(_ => softwareSpecRef)
      }
    } else {
      Future.successful(softwareSpec)
    }
  }

  def entityValidatePatchPayloadByPathName(patches: List[PatchPayloadElement],
                                           additionalPatchCheckMap: Option[Map[String, PatchPayloadElement => PatchPayloadElement]] = None): List[PatchPayloadElement] = {
    if (patches.isEmpty) {
      val msg = "No patches found, empty patch body."
      throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
    // patch.path ->  (PatchPayloadElement) => patch.path
    val patchMap: Map[String, PatchPayloadElement => PatchPayloadElement] = Map(
      "/tags" -> {
        (patch: PatchPayloadElement) => {
          patch.copy(path = s"/metadata${patch.path}",
            value = checkJsonArrayString(value = patch.value, fieldName = "tags", notEmpty = true))
        }
      },
      "/name" -> {
        (patch: PatchPayloadElement) => {
          if (patch.op.equals("remove")) {
            removeError("name")
          } else {
            patch.copy(path = s"/metadata${patch.path}",
              value = checkJsonString(value = patch.value, fieldName = "name", noeEmpty = true, checkForUnsafeCharacters = true))
          }
        }
      },
      "/description" -> {
        (patch: PatchPayloadElement) => {
          patch.copy(path = s"/metadata${patch.path}",
            value = checkJsonString(value = patch.value, fieldName = "description", checkForUnsafeCharacters = true))
        }
      },
      "/metadata/tags" -> {
        (patch: PatchPayloadElement) => {
          patch.copy(value = checkJsonArrayString(value = patch.value, fieldName = "tags", notEmpty = true))
        }
      },
      "/metadata/name" -> {
        (patch: PatchPayloadElement) => {
          if (patch.op.equals("remove")) {
            removeError("name")
          } else {
            patch.copy(value = checkJsonString(value = patch.value, fieldName = "name", noeEmpty = true, checkForUnsafeCharacters = true))
          }
        }
      },
      "/metadata/description" -> {
        (patch: PatchPayloadElement) => {
          patch.copy(value = checkJsonString(value = patch.value, fieldName = "description", checkForUnsafeCharacters = true))
        }
      }
    ) ++ additionalPatchCheckMap.getOrElse(Map())

    patches.map(patch => {
      val path = patch.path
      if (patchMap.contains(path)) {
        patchMap(path)(patch)
      } else {
        // special case for custom
        if (path.startsWith("/custom"))
          patch
        else {
          val msg = s"Patch path '${patch.path}' not allowed."
          throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
        }
      }
    })
  }

  def removeError(fieldName: String): Nothing = {
    val msg = s"The patch operation to remove '$fieldName' field is not supported."
    throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
  }

  def unsupportedPatchOperationError(fieldName: String, operation: String): Nothing = {
    val msg = s"The patch operation to $operation '$fieldName' field is not supported."
    throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
  }

  private def checkJsonString(value: Option[JsValue], fieldName: String, noeEmpty: Boolean = false, checkForUnsafeCharacters: Boolean = false): Option[JsValue] = {
    value.map {
      case v: JsString =>
        if (noeEmpty && v.value.trim.isEmpty) {
          blankError(fieldName)
        }
        if (checkForUnsafeCharacters) {
          unsafeCharactersValidator(v.convertTo[String], fieldName, noeEmpty)
        }
        v.copy(value = v.value.trim).convertTo[JsValue]
      case _ =>
        val msg = s"'$fieldName' fields need to be type string"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }

  private def checkJsonArrayString(value: Option[JsValue], fieldName: String, notEmpty: Boolean = false): Option[JsValue] = {
    value.map {
      case v: JsArray =>
        val array = v.elements.map {
          case s: JsString =>
            if (notEmpty && s.value.trim.isEmpty) {
              blankError(s"String value inside '$fieldName' array")
            }
            s.copy(value = s.value.trim).convertTo[JsValue]
          case _ =>
            val msg = s"'$fieldName' fields need to be type array of string"
            throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
        }
        JsArray(array).convertTo[JsValue]
      case _ =>
        val msg = s"'$fieldName' fields need to be type array of string"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
    }
  }

  def deleteAllAttachmentsIfExist(resource: Asset, camsClient: CAMSClient)(implicit ex: ExecutionContext, callContext: CallContext): Future[Unit] = {
    val contentExist = resource.attachments.exists(_.nonEmpty)
    if (contentExist) {
      val (assetId, attachments) = extractInfoFromAsset(resource)
      deleteAttachments(assetId, attachments.map(_.attachmentId), camsClient)
    } else Future.successful(())
  }

  def deleteAttachments(assetId: String, attachments: Vector[String], camsClient: CAMSClient)(implicit ex: ExecutionContext, callContext: CallContext): Future[Unit] = {
    for (
      _ <- seqFutures(attachments) { attachment =>
        camsClient.deleteAssetAttachment(callContext.identity, assetId, attachment, callContext.container, callContext.containerStorageType)
      }
    ) yield {}
  }

  def extractInfoFromAsset(resource: Asset): (String, Vector[AssetAttachmentMetadata]) = {
    val assetId = resource.metadata.assetId.getOrElse("")
    val attachments = resource.attachments.getOrElse(Vector[AssetAttachmentMetadata]())
    (assetId, attachments)
  }

  def entityTrainingDataReferencesValidator(trainingDataReferences: Vector[DataReference]): Vector[DataReference] = {
    //Connection cannot have a JSON at the second level
    trainingDataReferences.foreach(tdr =>
      connectionValidator(tdr.connection)
    )
    trainingDataReferences
  }

  def ObjectLocationValidator(objectLocation: ObjectLocation): ObjectLocation = {
    connectionValidator(objectLocation.connection)
    objectLocation
  }

  def connectionValidator(connection: Option[Map[String, JsValue]]): Unit = {
    connection.foreach { conn =>
      val jsonChildElements = conn.values.filter(_.isInstanceOf[JsObject]).flatMap(_.asJsObject.fields.filter(_._2.isInstanceOf[JsObject])).toVector
      if (jsonChildElements.nonEmpty) {
        val msg = s"Connection values cannot contain nested JSON elements"
        throw ServiceException(StatusCodes.BadRequest, InvalidRequestEntityMessage(msg))
      }
    }
  }

  def checkHardwareSpecExistInPipelineRef(pipeline: Option[PipelineRef]): Boolean = {
    pipeline.flatMap(_.hardwareSpec).map(_ => {
      return true
    })
    pipeline.flatMap(_.hybridPipelineHardwareSpecs).map(hybrid => {
      hybrid.map(_.hardwareSpec).map(_ => {
        return true
      })
    })
    false
  }

  def checkHardwareSpecExistInModelDefinitionRef(modelDefinition: Option[ModelDefinitionRef]): Boolean = {
    modelDefinition.flatMap(_.hardwareSpec).map(_ => {
      return true
    })
    false
  }

  def checkSoftwareSpecExistInModelDefinitionRef(modelDefinition: Option[ModelDefinitionRef]): Boolean = {
    modelDefinition.flatMap(_.softwareSpec).map(_ => {
      return true
    })
    false
  }

  def checkHardwareSpecExistInFederatedLearning(federatedLearning: Option[FederatedLearning]): Boolean = {
    federatedLearning.flatMap(_.hardwareSpec).map(_ => {
      return true
    })
    false
  }

  def validateHybridPipelineHardwareSpecs(hybridPipelineHardwareSpecs: Option[Vector[HybridPipelineHardwareSpec]],
                                          hardwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                          hardwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)]):
  Option[Vector[HybridPipelineHardwareSpec]] = {
    hybridPipelineHardwareSpecs.map(hybridHwSpecs => {
      hybridHwSpecs.map(hybridHwSpec => {
        hybridHwSpec.copy(hardwareSpec = validateHardwareSpecs(hybridHwSpec.hardwareSpec, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap))
      })
    })
  }

  def validatePipelineRef(pipeline: Option[PipelineRef],
                          hardwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                          hardwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)]): Option[PipelineRef] = {
    pipeline.map(pipelineRef => {
      val hardwareSpecRefCopy = pipelineRef.hardwareSpec.map(hdSpec => {
        validateHardwareSpecs(hdSpec, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      })
      val hybridPipelineHardwareSpecCopy = validateHybridPipelineHardwareSpecs(pipelineRef.hybridPipelineHardwareSpecs,
        hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      pipelineRef.copy(hardwareSpec = hardwareSpecRefCopy, hybridPipelineHardwareSpecs = hybridPipelineHardwareSpecCopy)
    })
  }

  def validateFederatedLearning(federatedLearning: Option[FederatedLearning],
                                hardwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                hardwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)]): Option[FederatedLearning] = {
    federatedLearning.map(federatedLearningRef => {
      val hardwareSpecRefCopy = federatedLearningRef.hardwareSpec.map(hdSpec => {
        validateHardwareSpecs(hdSpec, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      })
      federatedLearningRef.copy(hardwareSpec = hardwareSpecRefCopy)
    })
  }

  def validateModelDefinitionRef(modelDefinition: Option[ModelDefinitionRef],
                                 hardwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                 hardwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)],
                                 softwareSpecNameToIdMap: Map[String, Vector[(String, baseOrDerivedType)]],
                                 softwareSpecIdToTypeMap: Map[String, (String, baseOrDerivedType)]): Option[ModelDefinitionRef] = {
    modelDefinition.map(modelDef => {
      val modelDefHardwareSpecRefCopy = modelDef.hardwareSpec.map(hdSpec => {
        validateHardwareSpecs(hdSpec, hardwareSpecNameToIdMap, hardwareSpecIdToTypeMap)
      })
      val modelDefSoftwareSpecRefCopy = modelDef.softwareSpec.map(sfSpec => {
        validateSoftwareSpecs(sfSpec, softwareSpecNameToIdMap, softwareSpecIdToTypeMap)
      })
      modelDef.copy(hardwareSpec = modelDefHardwareSpecRefCopy, softwareSpec = modelDefSoftwareSpecRefCopy)
    })
  }

  def getSWHWSpecHelperMap(hardwareSpecExistent: Boolean,
                           softwareSpecExistent: Boolean,
                           envClient: EnvironmentsClient)(implicit callContext: CallContext, ec: ExecutionContext):
  (Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])],
    Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])]) = {
    (getHWSpecHelperMap(hardwareSpecExistent, envClient),
      getSWSpecHelperMap(softwareSpecExistent, envClient))
  }

  def getSWSpecHelperMap(softwareSpecExistent: Boolean,
                         envClient: EnvironmentsClient)(implicit callContext: CallContext, ec: ExecutionContext):
  Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] = {
    val softwareSpecHelperMaps: Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] =
      if (softwareSpecExistent)
        getSoftwareSpecHelperMaps(envClient)
      else Future.successful((Map(), Map()))
    softwareSpecHelperMaps
  }

  def getHWSpecHelperMap(hardwareSpecExistent: Boolean,
                         envClient: EnvironmentsClient)(implicit callContext: CallContext, ec: ExecutionContext):
  Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] = {
    val hardwareSpecHelperMaps: Future[(Map[String, Vector[(String, baseOrDerivedType)]], Map[String, (String, baseOrDerivedType)])] =
      if (hardwareSpecExistent)
        getHardwareSpecHelperMaps(envClient)
      else Future.successful((Map(), Map()))
    hardwareSpecHelperMaps
  }

}

object baseOrDerivedType extends Enumeration {
  type baseOrDerivedType = Value
  val BASE: Value = Value("base")
  val DERIVED: Value = Value("derived")
}
