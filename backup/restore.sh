#! /bin/sh

set -e

tarsnap -x -f "cubemania-database-2016-10-03T00-36-22"
pg_restore --verbose --clean --no-acl --no-owner -h db -U postgres -d cubemania_production /data/db.dump
