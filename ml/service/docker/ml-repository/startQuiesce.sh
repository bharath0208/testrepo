#!/usr/bin/env bash

pid=$(ps -ef | grep 'MLRepositoryApp' | grep -v grep | awk '{print $2}' | head -1)

echo "$pid"

if [ -z "$pid" ]
then
echo "MLRepositoryApp is not running!"
exit 1
else
echo "Starting quiesce for pid : $pid"
# use -INT for SIGINT
kill -INT "$pid"
fi
