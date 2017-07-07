#! /bin/sh
set -e

docker build -f nginx/Dockerfile -t timhabermaas/cubemania:nginx .
docker build -t timhabermaas/cubemania:rails_old .
docker build -f backup/Dockerfile -t timhabermaas/cubemania:backup backup
docker push timhabermaas/cubemania:nginx
docker push timhabermaas/cubemania:rails_old
docker push timhabermaas/cubemania:backup
