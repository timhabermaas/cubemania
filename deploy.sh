#! /bin/sh
set -e

env DOCKER_BUILDKIT=1 docker-compose -f docker-compose.yml -f docker-compose.prod.yml build web
docker-compose -f docker-compose.yml -f docker-compose.prod.yml run web bundle exec rake assets:precompile
env DOCKER_BUILDKIT=1 docker-compose -f docker-compose.yml -f docker-compose.prod.yml build
