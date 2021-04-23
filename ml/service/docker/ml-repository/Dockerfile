#!/bin/bash -x
#-------------------------------------------------------------
# IBM Confidential
# OCO Source Materials
# (C) Copyright IBM Corp. 2016
# The source code for this program is not published or
# otherwise divested of its trade secrets, irrespective of
# what has been deposited with the U.S. Copyright Office.
#-------------------------------------------------------------
#- Docker file for preparation of WML ML V4 Repository Service installation -#
#- It installs all prerequisite libraries and configures it -#

#- Set the base image (jdk11) -#
ARG BASE_IMAGE=wmlbaseubi8minimal
ARG VERSION=1.0.x
FROM registry.ng.bluemix.net/wmlregistry/$BASE_IMAGE:latest AS builder
ARG ARG_DEPLOYMENT_IMAGE_PLATFORM

#- Create required directories -#
RUN export TERM=xterm \
    && mkdir -p /licenses \
    /opt/ibm/ml-repository/logs \
    /opt/ibm/ml-repository/bin \
    /opt/ibm/ml-repository/conf \
    /opt/ibm/ml-repository/lib \
    /opt/ibm/ml-repository/migration-lib \
    /opt/ibm/ml-repository/keys \
    /opt/ibm/ml-repository/certs \
    /opt/ibm/ml-repository/utillibs \
    /etc/mt-logstash-forwarder/config

#- Copy required JARs/scripts -#
COPY docker/target/service-*.jar docker/target/lib/*.jar /opt/ibm/ml-repository/lib/
COPY docker/target/migration-*.jar docker/target/migration-lib/*.jar /opt/ibm/ml-repository/migration-lib/
COPY docker/ml-repository/*.sh /opt/ibm/ml-repository/bin/

#- Copy required keys -#
COPY docker/ml-repository/keys/keystore.jks docker/ml-repository/keys/mltoken.pub /opt/ibm/ml-repository/keys/

#- Copy NewRelic JAR and other config files -#
RUN cp /tmp/logdrain/ngpservice-hydra.conf /etc/mt-logstash-forwarder/config/ngpservice-hydra.conf

#- change ownership to wmluser and set file protections -#
RUN chmod -R 755 /tmp/logdrain/updateconf_wml.sh \
    && chmod -R 755 /opt/ibm/ml-repository/lib/*.jar /opt/ibm/ml-repository/migration-lib/*.jar /opt/ibm/ml-repository/bin/*.sh \
    && chmod -R 777 /opt/ibm/ml-repository/logs /opt/ibm/ml-repository/certs \
    && chmod -R 755 /opt/ibm/ml-repository/conf

#- SECOND STAGE                    -#
#- now build the repository docker -#
FROM registry.ng.bluemix.net/wmlregistry/$BASE_IMAGE:latest

# do this at beginning as always the same
RUN export TERM=xterm \
    && mkdir -p /licenses \
    /etc/mt-logstash-forwarder/config

#- Copy licenses for UBI (do here as should not change often) -#
COPY docker/ml-repository/licenses /licenses

COPY --from=builder /etc/mt-logstash-forwarder/config /etc/mt-logstash-forwarder/config
COPY --from=builder /tmp/logdrain /tmp/logdrain
COPY --from=builder /opt/ibm /opt/ibm

#- Install Dependencies -#
# this is part of the base image (Rayanki 7 Aug 2020) for x86
# RUN yum install -y tcpdump
RUN if [ "${ARCH}" == "ppc64le" ] ; then wget -O /opt/ibm/ml-repository/utillibs/tcpdump-4.9.2-9.fc31.ppc64le.rpm  https://rpmfind.net/linux/fedora-secondary/releases/31/Everything/ppc64le/os/Packages/t/tcpdump-4.9.2-9.fc31.ppc64le.rpm; \
    rpm -ivh /opt/ibm/ml-repository/utillibs/tcpdump-4.9.2-9.fc31.ppc64le.rpm; \
    rpm -Fvh /opt/ibm/ml-repository/utillibs/*.rpm; fi
RUN if [ "${ARCH}" == "s390x" ] ; then wget -O /opt/ibm/ml-repository/utillibs/tcpdump-4.9.3-2.fc32.s390x.rpm https://rpmfind.net/linux/fedora-secondary/releases/32/Everything/s390x/os/Packages/t/tcpdump-4.9.3-2.fc32.s390x.rpm; \
    rpm -ivh /opt/ibm/ml-repository/utillibs/tcpdump-4.9.3-2.fc32.s390x.rpm; \
    rpm -Fvh /opt/ibm/ml-repository/utillibs/*.rpm; fi
LABEL "name"="ML Repository Service" \
      "vendor"="IBM" "version"=$VERSION \
      "release"=$VERSION \
      "summary"="ML Repository Service V4 for Watson Machine Learning" \
      "description"="This is the image for the ML Repository Service V4"

USER wmluser
#- Run service -#
CMD 'sh' -C '/opt/ibm/ml-repository/bin/runService.sh';'bash'
