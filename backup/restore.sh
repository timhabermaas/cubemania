#! /bin/sh

set -e

tarsnap -x -v -f $1
pg_restore --verbose --clean --no-acl --no-owner -h db -U postgres -d cubemania_production /data/db.dump
