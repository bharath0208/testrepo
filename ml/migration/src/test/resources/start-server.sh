#!/bin/bash
set -x
export DEVELOPMENT=true
exec "${java.home}/bin/java" -DUSER="${user.name}" -Dakka.http.service.interface=localhost "${log.file}" "${config.file}" -cp "${runtime_classpath}" ${main.class}
