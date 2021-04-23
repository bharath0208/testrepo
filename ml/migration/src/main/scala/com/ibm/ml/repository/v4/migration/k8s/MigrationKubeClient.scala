/*
 * IBM Confidential
 * OCO Source Materials
 * 5737-H76, 5725-W78, 5900-A1R
 * (C) Copyright IBM Corp. 2017-2020
 *
 * The source code for this program is not published or otherwise divested of its trade secrets,
 * irrespective of what has been deposited with the U.S. Copyright Office.
 */

package com.ibm.ml.repository.v4.migration.k8s

import akka.actor.ActorSystem
import com.ibm.analytics.wml.kubeclient.errors.Errors._
import com.ibm.analytics.wml.kubeclient.models.EnvParam.{ConfigMapKeyParamValue, SecretKeyParamValue}
import com.ibm.analytics.wml.kubeclient.models.KubeClientConstants.SERVICE_NAME
import com.ibm.analytics.wml.kubeclient.models.{EnvParam, _}
import com.ibm.analytics.wml.kubeclient.{KubernetesClient, RetryFailure}
import com.ibm.ml.repository.v4.migration.service.MigrationServiceContext
import com.ibm.ml.repository.v4.utils._
import com.ibm.ml.repository.v4.utils.logging.ExceptionLogger
import com.typesafe.scalalogging.StrictLogging
import skuber.EnvFromSource.SecretEnvSource
import skuber.LabelSelector.IsEqualRequirement
import skuber.batch.JobList
import skuber.{EnvFromSource, LabelSelector, Volume}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Try

object MigrationKubeClient {

  implicit class ContainerLogger(container: Container) {
    def asString(): String = {
      s"Container(name= ${container.name} image= ${container.image} resources = ${container.resources}) env = ${
        container.env.map {
          envParam => {
            val envVameLc = envParam.name.toLowerCase
            if (envVameLc.contains("cert") || envVameLc.contains("key") || envVameLc.contains("pass") || envVameLc.contains("token"))
              EnvParam(envParam.name, "***")
            else
              envParam
          }
        }
      } envFromSource =  ${container.envFromSource}, ports = ${container.ports} command = ${container.command}"
    }
  }

}

case class MigrationKubeClient()(implicit actorSystem: ActorSystem, msc: MigrationServiceContext) extends StrictLogging {
  implicit val ec: ExecutionContextExecutor = actorSystem.dispatcher
  val kubeClient: KubernetesClient = KubernetesClient("Migration Service", RetryFailure(2, 50))
  val jobPrefix = "migration-v4-job-"
  val CREATE_JOB_TIMEOUT: FiniteDuration = getDuration("service.migration-job.create-timeout", 10.minutes)
  val migrationJobServiceName = "migration-job"

  private def getEnvOrPlainString(envKey: String, confKey: String): String = {
    sys.env.get(envKey) match {
      case Some(value) => value
      case _ => config.getString(confKey)
    }
  }

  def getJobName(jobId: String): String = s"$jobPrefix$jobId"

  def getIdFromJobName(jobName: String): String = jobName.replaceAll(jobPrefix, "")

  def getJobsByServiceName(serviceName: String): Future[JobList] = {
    import skuber.json.batch.format.jobListFmt
    for {
      jobList <- kubeClient.getKubeInstance.listSelected[JobList](LabelSelector(IsEqualRequirement(SERVICE_NAME, serviceName)))
    } yield {
      jobList
    }
  }

  def createJob(migrationJobId: String,
                jobDetails: String,
                serviceVersion: Option[String] = None,
                delOnError: Boolean = true): Future[String] = {

    try {

      val mem = Try(config.getString("service.migration-job.memory")).getOrElse("1024Mi")
      val cpu = Try(config.getDouble("service.migration-job.cpu")).getOrElse(1.0)
      logger.info(s"createJob: $migrationJobId will have memory $mem and cpu $cpu")
      // to do for local we need to add
      val defaultVersion = Try(config.getString("service.migration-job.default.version")) getOrElse msc.implementationVersion.get

      val jobName = getJobName(migrationJobId)

      val commonEnvVars = List(
        // add more env var here
        EnvParam("MEMORY", mem.replaceAll("Mi", "")),
        EnvParam("DEPLOYMENT_PLATFORM", environment),
        EnvParam("WMLENV", wmlEnv),
        EnvParam("DATACENTRE", dataCenter),
        EnvParam("MIGRATION_JOB_ID", migrationJobId),
        EnvParam("JOB_DETAILS", jobDetails)
      )

      val allEnvVars = if (isPrivateCloud) {
        // local
        // todo local is untested will do it later
        List(
          EnvParam("DEPLOYMENT_PRIVATE", sys.env.getOrElse("DEPLOYMENT_PRIVATE", "local")),
          EnvParam("INTERNAL_NGINX_CERT", SecretKeyParamValue("certificate.pem", "internal-nginx-svc-tls")),
          EnvParam("WDP_BASE_URL", ConfigMapKeyParamValue("host-url", "wdp-config")),
          // here we are using the same key store as the training
          EnvParam("KEYSTORE_PASSWORD", SecretKeyParamValue("key_store_password", "training-secrets")),
          EnvParam("KUBERNETES_SERVICE_HOST", sys.env.getOrElse("KUBERNETES_SERVICE_HOST", "")),
          EnvParam("KUBERNETES_SERVICE_PORT", sys.env.getOrElse("KUBERNETES_SERVICE_PORT", "")),
          EnvParam("WML_CLOUDANT_USERNAME", SecretKeyParamValue("username", "wdp-cloudant-creds")),
          EnvParam("WML_CLOUDANT_PASSWORD", SecretKeyParamValue("password", "wdp-cloudant-creds")),
          EnvParam("WML_CLOUDANT_HOSTNAME", ConfigMapKeyParamValue("cloudant_host", "wmlrepositoryv4configmap"))
        )
      } else {
        // cloud
        List(
          EnvParam("KUBECONFIG", "/opt/ibm/wmlkubeconfig/kubeconfig.yml"),
          EnvParam("CRED_ENCRYPT_KEY", sys.env.getOrElse("CRED_ENCRYPT_KEY", "")),
          EnvParam("WML_ML_REPOSITORY_SVC_KEYSTOREPWD", SecretKeyParamValue("WML_ML_REPOSITORY_SVC_KEYSTOREPWD", "wml-repository-v4-svc-secret"))
        )
      } ::: commonEnvVars

      // remove empty env vars
      val envVars = allEnvVars.filter(_.value.value != "")
      val envFromSource = if (isPrivateCloud) {
        List(
          EnvFromSource(source = SecretEnvSource("wdp-service-id"))
        )
      } else {
        List(
          EnvFromSource(source = SecretEnvSource("wml-stable-secret")),
          EnvFromSource(source = SecretEnvSource("wml-cloudant-secret"))
        )
      }
      val dockerName = s"wml-migration-$migrationJobId"

      val version = serviceVersion.getOrElse(defaultVersion)
      val image = getEnvOrPlainString("Migration_JOB_IMAGE_NAME", "service.migration-job.image") + ":" + version
      logger.info(s"Running kube job with image: $image")

      logger.debug(s"Using pod specifications ( limit = mem: $mem ; cpu: $cpu  requested = mem: $mem ; cpu: ${cpu / 3})")
      val resources = Resources(limits = ResourceParam(cpu, mem), requests = ResourceParam(cpu / 3, mem))
      val command = List("sh", "-C", "/opt/ibm/ml-repository/bin/runMigrationJob.sh")
      val containers = Seq(Container(name = dockerName,
        image = image,
        resources = Some(resources),
        env = envVars,
        envFromSource = envFromSource,
        ports = List(),
        command = command
      ))

      import MigrationKubeClient.ContainerLogger
      logger.debug(s"Try creating job for migration-job from containers list [${containers.map(_.asString()).mkString}]")
      val secrets = if (isPrivateCloud)
        List.empty
      else
        List(ImagePullSecret(getEnvOrPlainString("IMAGE_PULL_SECRETS", "service.migration-job.secret-name")))

      val volumeMounts = if (isPrivateCloud) {
        Some(List(
          Volume.Mount(mountPath = "/opt/ibm/ml-repository/conf", name = "config-volume"),
          Volume.Mount(mountPath = "/user-home", name = "user-home-mount", readOnly = true, subPath = "_global_/config/jwt/")
        ))
      } else {
        Some(List(
          Volume.Mount(mountPath = "/opt/ibm/ml-repository/conf", name = "config-volume")
        ))
      }
      val volumes = if (isPrivateCloud) {
        Some(List(
          Volume(
            name = "config-volume",
            source = Volume.ConfigMapVolumeSource(defaultMode = Some(420),
              name = "wmlrepositorymigrationv4configmap")
          ),
          Volume(
            name = "user-home-mount",
            source = Volume.PersistentVolumeClaimRef(claimName = "user-home-pvc")
          )
        ))
      } else {
        Some(List(
          Volume(
            name = "config-volume",
            source = Volume.ConfigMapVolumeSource(defaultMode = Some(420),
              name = "wmlrepositorymigrationv4configmap")
          )
        ))
      }

      val labels = Map(
        "appType" -> "custom-coded",
        SERVICE_NAME -> migrationJobServiceName,
        "securityCategory" -> "internal-private")

      logger.debug(s"Trying to create Kube Job with migration job id $migrationJobId, and job name $jobName ")
      val jobAnnotation = if (isPrivateCloud)
        Some(Map(
          "cloudpakName" -> "IBM Cloud Pak for Data",
          "cloudpakId" -> "eb9998dcc5d24e3eb5b6fb488f750fe2",
          "cloudpakVersion" -> "3.0.0",
          "productName" -> "IBM Watson Machine Learning for IBM Cloud Pak for Data",
          "productID" -> "ICP4D-Watson-Machine-Learning-3-0-0",
          "productVersion" -> "3.0.0",
          "productMetric" -> "VIRTUAL_PROCESSOR_CORE",
          "productChargedContainers" -> "All"))
      else None

      (for {
        podIp <- kubeClient.createJob(jobName,
          serviceName = migrationJobServiceName,
          spec = ContainerSpec(containers.toList, secrets),
          labels = Some(labels),
          annotations = jobAnnotation,
          podCreationTimeoutInSec = CREATE_JOB_TIMEOUT.toSeconds,
          nodeAffinityLabelValue = None,
          volumeMounts = volumeMounts,
          volumes = volumes,
          restartPolicy = skuber.RestartPolicy.Never,
          securityContext = None,
          mountServiceAccount = true,
          withLogger = true,
          ttlSecondsAfterJobFinished = Some(24 * 60 * 60),
          waitForResources = true
          // clean up after 1 day in case orchestrator does not manage to delete it via its scheduler (set once per hour)
        )
      } yield {
        logger.debug(s"Successfully created job pod for $jobName on $podIp")
        jobName
      }).recoverWith {
        case f =>
          ExceptionLogger.log(Some("Unable to launch for pod"), f, None, callStack = true)
          //async call to delete job
          if (delOnError) {
            (for {
              msg <- kubeClient.deleteJob(jobName)
            } yield {
              logger.info(s"Failed Kube job $jobName terminated: $msg")
            }).recover {
              case e =>
                ExceptionLogger.log(Some("Failed Kube job $jobName could not be terminated"), e, None, callStack = true)
            }
          }
          f match {
            case i: InsufficientResourcesException => Future.failed(i.failureSequence)
            case i: PodCreationFailureException => Future.failed(i.failureSequence)
            case i: InvalidServiceNameException => Future.failed(i.failureSequence)
            case i: InstanceNotFoundException => Future.failed(i.failureSequence)
            case i: InfrastructureFailureException => Future.failed(i.failureSequence)
            case _ => Future.failed(f)
          }
      }
    }
    catch {
      case e: Throwable =>
        ExceptionLogger.log(Some(s"Failed to launch job id $migrationJobId"), e, None, callStack = true)
        Future.failed(e)
    }
  }

  def deleteJobByName(Name: String): Future[Any] = {
    logger.debug(s"trying to delete job $Name")
    (for {
      job <- kubeClient.deleteJob(Name)
    } yield {
      logger.debug(s"job $Name deleted")
      job
    }) recover {
      case e =>
        ExceptionLogger.log(Some(s"Error on delete kube job $Name"), e, None, callStack = true)
    }
  }
}
