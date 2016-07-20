#! /bin/sh

docker build . -t cubemania-api
docker run --rm -ti cubemania-api
