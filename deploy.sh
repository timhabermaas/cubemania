#! /bin/sh
set -e

docker-compose run web bundle exec rake assets:precompile
docker-compose build
docker-compose push
