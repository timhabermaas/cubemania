#! /bin/sh
set -e

docker-compose -f docker-compose.yml -f docker-compose.prod.yml build web
docker-compose -f docker-compose.yml -f docker-compose.prod.yml run web bundle exec rake assets:precompile
docker-compose -f docker-compose.yml -f docker-compose.prod.yml build
