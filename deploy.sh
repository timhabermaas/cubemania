#! /bin/sh
set -e

docker-compose run web bundle exec rake assets:precompile
docker build -f nginx/Dockerfile -t timhabermaas/cubemania_nginx .
docker build -t timhabermaas/cubemania_rails .
docker build -f backup/Dockerfile -t timhabermaas/cubemania_backup backup
docker build -f records/Dockerfile -t timhabermaas/cubemania_records records
docker push timhabermaas/cubemania_nginx
docker push timhabermaas/cubemania_rails
docker push timhabermaas/cubemania_backup
docker push timhabermaas/cubemania_records
