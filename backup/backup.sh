#! /bin/sh

set -e

pg_dump -Fc -h db -U postgres cubemania_production > /data/db.dump
tarsnap -c -f "cubemania-database-$(date +%Y-%m-%d_%H-%M-%S)" /data/db.dump
