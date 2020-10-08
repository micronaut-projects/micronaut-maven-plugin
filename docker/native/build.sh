#!/usr/bin/env bash

docker build -t alvarosanchez/micronaut-docker-native:latest .
docker build -t alvarosanchez/micronaut-native-image-builder:20.2.0-java11 -f DockerfileBuild .
docker push alvarosanchez/micronaut-native-image-builder:20.2.0-java11
docker push alvarosanchez/micronaut-docker-native:latest