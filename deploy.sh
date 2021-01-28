#! /bin/sh
set -e

COMMIT_SHA=$(git rev-parse HEAD)
echo $COMMIT_SHA

docker-compose run web bundle exec rake assets:precompile
docker build -f nginx/Dockerfile -t timhabermaas/cubemania_nginx:$COMMIT_SHA .
docker build -t timhabermaas/cubemania_rails:$COMMIT_SHA .
docker build -f backup/Dockerfile -t timhabermaas/cubemania_backup:$COMMIT_SHA backup
docker build -f records/Dockerfile -t timhabermaas/cubemania_records:$COMMIT_SHA records
docker push timhabermaas/cubemania_nginx:$COMMIT_SHA
docker push timhabermaas/cubemania_rails:$COMMIT_SHA
docker push timhabermaas/cubemania_backup:$COMMIT_SHA
docker push timhabermaas/cubemania_records:$COMMIT_SHA
