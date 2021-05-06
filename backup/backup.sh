#! /bin/sh

set -e

file=/data/cubemania-database-$(date +%Y-%m-%d_%H-%M-%S).dump.gz

# `-Fc`: custom format suitable for pg_restore.
pg_dump -Fc -h db -U postgres cubemania_production | gzip > $file
aws s3 cp $file s3://${AWS_BUCKET_NAME}/
rm $file
