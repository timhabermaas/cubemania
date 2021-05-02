#! /bin/sh

set -e

filename=$1

target=/data/current.dump.gz

aws s3 cp s3://backup.cubemania.org/$filename $target
gzip -d $target
pg_restore --verbose --clean --no-acl --no-owner -h db -U postgres -d cubemania_production /data/current.dump
