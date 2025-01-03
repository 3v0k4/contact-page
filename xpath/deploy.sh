#!/bin/bash

set -e

yarn compile
sed -i '' -e "s/VERSION/$(date +%s)/g" public/index.html

# rsync \
#   --rsync-path="sudo rsync" \
#   -a \
#   public/ riccardo@odone.io:/usr/share/nginx/html/odone.io/xpath/

aws s3 sync ./public s3://odone.io/xpath/
aws cloudfront create-invalidation \
    --distribution-id EHEV8XRM85R9X \
    --paths "/xpath*"
git checkout public/index.html
