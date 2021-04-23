#!/usr/bin/env bash

export JAVA_HOME=/usr/lib/jvm/java

echo $WMLENV > /tmp/env.out

#################################################
# Configure service according to the env
#################################################


if [ "$WMLENV" != "prod" ];then
  nohup tcpdump port 12550 -vvv -s0 -w /opt/ibm/ml-repository/logs/tcpdump-deployment.pcap &
fi

export AVAILABLE_MEM=${MEMORY:-1024}
export VM_MEM=$(( ${AVAILABLE_MEM}-200 ))

$JAVA_HOME/bin/java \
   -classpath "/opt/ibm/ml-repository/migration-lib/*" \
   -Dlog4j.configurationFile=file:/opt/ibm/ml-repository/conf/log4j2.xml \
   -Dconfig.file=/opt/ibm/ml-repository/conf/ml-repository-migration-service.conf \
   -Xms${VM_MEM}m -Xmx${VM_MEM}m \
    com.ibm.ml.repository.v4.migration.upgrade.job.MLRepositoryDeleteJobApp rollback
