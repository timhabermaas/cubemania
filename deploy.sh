#! /bin/sh
set -e

docker-compose build web
docker-compose run web bundle exec rake assets:precompile
docker-compose build
