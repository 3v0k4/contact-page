#!/usr/bin/env bash
set -euxo pipefail

yarn build
rsync \
  --rsync-path="sudo rsync" \
  -ca \
  out/ riccardo@odone.io:/usr/share/nginx/html/odone.io/
aws s3 sync ./out s3://odone.io
aws cloudfront create-invalidation \
    --distribution-id EHEV8XRM85R9X \
    --paths "/*"
