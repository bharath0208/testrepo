# ml-repository
This is the repository for the WML service that handles the V4 (final) repository that will handle the repository related API's under the path `/ml/v4`.

## Github epic
https://github.ibm.com/NGP-TWC/ml-planning/issues/14479

## Jenkins build 
http://jenkins-wml-server1.fyre.ibm.com:8080/job/WML-Repository-V4_publish/

## Build environment

In order to allow the tests to run locally you must have the
file `${user.home}/ml-repository/local.conf`. This file is
not checked into `git` because it contains secrets and it
should not be in the service configuration defaults.

A typical `local.conf` file looks like this:
```hocon
service {
  validateTokenService.ml.pubkey.path = ../keystore/mykey_pub.pem
  ml-repository {
    log {
      pretty-print = true
    }
    v4-services {
      log-level = INFO
    }
  }

  ssl {
    port = 12555
    keystore {
      path = ../keystore/keystore.jks
      type = JKS
      password = "xxxxxxxxxxxxxxx"
    }
  }

  iam {
    url = "https://iam.test.cloud.ibm.com/"
    pdp {
      serviceName = pm-20-devops
      apiKey = "xxxxxxxxxxxxxxxxxx"
    }
  }
}
```

In addition you must have the following environment variables set:
```
WML_API_KEY=dPjP-6TMHTWq3GWnQ9lnJ4q9xI9xxxxxxxxxxxxxxx
WML_COS_CRN="crn:v1:staging:public:cloud-object-storage:global:a/e0b2fcca717d083d17058ad089f41a2b:1c2cd57a-f52b-4fff-a496-xxxxxxxxxxxx::"
```

Ask one of the team members if you need help.

For point to cp4d cluster, please the link below

https://github.ibm.com/WML/ml-repository/wiki/Point-your-local-dev-env-to-cp4d-cluster

## Building the service

The service is built using maven:

```shell script
mvn clean install
```

This command will build the service and run the unit tests and integration tests.

### Building with code coverage

There is a profile called `coverage` that can be used to
create the code coverage during a build:
```shell script
mvn --activate-profiles coverage clean install
```

## Running (or debugging) the service locally

Running the service:
```shell script
cd service
mvn --activate-profiles run-local
```

Debugging the service:
```shell script
cd service
mvn --activate-profiles debug-local
```

Once the service is running - in the IntelliJ IDE run the
command `Run -> Attach to process`.

## Running the local tests using the WML service id

The WML service id will verify that the API endpoint being used is a `private`
endpoint. In order to simulate this on your local machine you need to add
the following line to the `/etc/hosts` (`sudo vi /etc/hosts`) as shown below:

```shell script
# For local testing
127.0.0.1 localhost-private
```

## How to update to the latest version of utils-lib libraries
1. Build locally with `-U` to get the latest version: `mvn clean install -U`
1. In the IntelliJ IDE - select the root project and then run (right mouse button) `Maven -> Reimport`.

## Tags for tests

Scalatest allows the creation of tags that can then be used to
run tests that have one or more tags or by excluding one or
more tags.

See [Tagging your tests](https://www.scalatest.org/user_guide/tagging_your_tests)
for details about `scalatest` tags.

A Java annotation tag can be created like this (note that this is a Java file):
```java
package com.ibm.analytics.wml.repository.ml.tags;

import java.lang.annotation.*;
import org.scalatest.TagAnnotation;

@TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface SmokeTest {}
```
This creates an annotation that can be used at the `class`
or `method` level.

The annotation can be used like this:
```
@SmokeTest
class AccessLoggerSpec extends AnyFeatureSpec {
  ...
}
```
A `scalatest` tag can then be derived like this:
```
package com.ibm.analytics.wml.repository.ml

import org.scalatest.Tag

object SmokeTest extends
  Tag("com.ibm.analytics.wml.repository.ml.tags.SmokeTest")
```
The `scalatest` tag can be used like this:
```
Feature("API Version") {
  Scenario("checking version", SmokeTest) {
    ...
  }
}
```

Once the tags are set then tests can be run by `including`
or `excluding` tags. This is controlled by properties in
the maven build and one way to implement this is by creating
a maven profile as shown below:
```
<profile>
  <id>smoke-tests</id>
  <properties>
    <tags.to.include>com.ibm.analytics.wml.repository.ml.tags.SmokeTest</tags.to.include>
  </properties>
</profile>
```

## Running the JMeter tests

#### Running the tests against `fvt`

```shell script
mvn -P jmeter-fvt
```

#### Running the tests against the local server

1. Start the local server in a terminal:
    ```shell script
    mvn -P run-local
    ```
1. Run the tests in a terminal:
   ```shell script
   mvn -P jmeter-local
   ```

## More information
Please check our wiki page https://github.ibm.com/WML/ml-repository/wiki
