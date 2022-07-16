#!/usr/bin/env bash
set -euxo pipefail

yarn build
rsync \
  --rsync-path="sudo rsync" \
  -ca \
  out/ riccardo@odone.io:/usr/share/nginx/html/odone.io/
