#!/usr/bin/env bash
set -ex

#maven
export MAVEN_VERSION=3.6.3
export SHA=c35a1803a6e70a126e80b2b3ae33eed961f83ed74d18fcd16909b2d44d7dada3203f1ffe726c17ef8dcca2dcaa9fca676987befeadc9b9f759967a8cb77181c0
export BASE_URL=https://apache.osuosl.org/maven/maven-3/${MAVEN_VERSION}/binaries

mkdir -p /usr/share/maven /usr/share/maven/ref \
  && curl -fsSL -o /tmp/apache-maven.tar.gz ${BASE_URL}/apache-maven-${MAVEN_VERSION}-bin.tar.gz \
  && echo "${SHA}  /tmp/apache-maven.tar.gz" | sha512sum -c - \
  && tar -xzf /tmp/apache-maven.tar.gz -C /usr/share/maven --strip-components=1 \
  && rm -f /tmp/apache-maven.tar.gz \
  && ln -s /usr/share/maven/bin/mvn /usr/bin/mvn \
  && mvn -version


#python3

python3 -m pip install --upgrade pip
pip3 install --no-cache-dir click \
    softlayer \
    markdown-generator \
    pyyaml

# install docker client
curl -fsSL get.docker.com | bash

# install kubectl client
curl -LO https://storage.googleapis.com/kubernetes-release/release/$(curl -s https://storage.googleapis.com/kubernetes-release/release/stable.txt)/bin/linux/amd64/kubectl
chmod +x ./kubectl
mv ./kubectl /usr/local/bin/kubectl

# install helm client
curl -L https://raw.githubusercontent.com/kubernetes/helm/master/scripts/get | bash

# install Bluemix client
curl -L https://clis.ng.bluemix.net/download/bluemix-cli/latest/linux64 | tar -xz
./Bluemix_CLI/install_bluemix_cli
