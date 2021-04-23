#!/usr/bin/env bash

export JAVA_HOME=/usr/lib/jvm/java

echo $WMLENV > /tmp/env.out

#################################################
# Configure service according to the env
#################################################

if [ "$WMLENV" != "prod" ];then
  nohup tcpdump port 12550 -vvv -s0 -w /opt/ibm/ml-repository/logs/tcpdump-deployment.pcap &
fi

# TEMPORARY
xlogopts=""
if [ "$WMLENV" = "fvt" ];then
  xlogopts="-Xloggc:/opt/ibm/ml-repository/logs/gctrace.log"
fi

export AVAILABLE_MEM=${MEMORY:-1024}
export VM_MEM=$(( ${AVAILABLE_MEM}-200 ))

$JAVA_HOME/bin/java \
   -classpath "/opt/ibm/ml-repository/lib/*" \
   -Dlog4j.configurationFile=file:/opt/ibm/ml-repository/conf/log4j2.xml \
   -Dconfig.file=/opt/ibm/ml-repository/conf/ml-repository-service.conf \
   -Xms${VM_MEM}m -Xmx${VM_MEM}m ${xlogopts} \
    com.ibm.ml.repository.v4.service.app.MLRepositoryApp
