#!/usr/bin/env bash
set -ex

mkdir -p ~/.ssh
ssh-keygen -q -t rsa -N '' -f ~/.ssh/id_rsa

sshpass -p "${REMOTE_DOCKER_PASSWORD}" ssh-copy-id -i ~/.ssh/id_rsa.pub -f -o StrictHostKeyChecking=no ${REMOTE_DOCKER_ADDRESS}

docker context create remote-docker --docker "host=ssh://${REMOTE_DOCKER_ADDRESS}"

docker buildx create --name remote-docker-builder remote-docker

docker context use remote-docker

docker buildx use remote-docker-builder

docker buildx inspect --bootstrap