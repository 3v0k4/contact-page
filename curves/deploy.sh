#!/bin/bash

set -e

rsync \
  --rsync-path="sudo rsync" \
  -a \
  public riccardo@odone.io:/usr/share/nginx/html/odone.io/
aws s3 sync public s3://odone.io/curves/
aws cloudfront create-invalidation \
    --distribution-id EHEV8XRM85R9X \
    --paths "/curves"
