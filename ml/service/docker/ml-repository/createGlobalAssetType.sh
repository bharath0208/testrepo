#!/usr/bin/env bash

export JAVA_HOME=/usr/lib/jvm/java


export AVAILABLE_MEM=${MEMORY:-1024}
export VM_MEM=$(( ${AVAILABLE_MEM}-200 ))

if [ "$DEPLOYMENT_PLATFORM" = "private" ];then
 export ENV=ICP
 export AUTH_TOKEN=`env | sed -n 's/service-id-credentials=\(.*\)/\1/p'`
 export SERVICE_URL=wmlproxyservice
else
 if [ "$WMLENV" = "prod" ] || [ "$WMLENV" = "ypcr" ] || [ "$WMLENV" = "ypqa" ];then
   export ENV=PROD
 else
   export ENV=TEST
 fi
  export AUTH_TOKEN=${WML_STABLE_API_KEY}
  export SERVICE_URL=${CAMS_SERVICE_URL}
fi

echo ENV = ${ENV} AUTH_TOKEN = ${AUTH_TOKEN} SERVICE_URL = ${SERVICE_URL}

${JAVA_HOME}/bin/java \
   -classpath "/opt/ibm/ml-repository/lib/*" \
   -Dlog4j.configurationFile=file:/opt/ibm/ml-repository/conf/log4j2.xml \
   -Xms${VM_MEM}m -Xmx${VM_MEM}m \
    com.ibm.ml.repository.v4.utils.cams.GlobalAssetTypesApp ${ENV} ${SERVICE_URL} ${AUTH_TOKEN}
