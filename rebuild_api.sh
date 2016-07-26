#! /bin/sh
set -e

cd api
./build.sh
cd ..
docker-compose build api
docker-compose restart api
