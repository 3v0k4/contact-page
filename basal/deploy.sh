#!/bin/bash

set -e

rm -rf dist/
mkdir dist/
npm install
npx tailwindcss -i input.css -o dist/output.css --minify
cp -rf public/* dist/
sed -i '' -e "s/VERSION/$(date +%s)/g" dist/index.html
rsync \
  --rsync-path="sudo rsync" \
  -a \
  dist/ riccardo@odone.io:/usr/share/nginx/html/basal.odone.io/
aws s3 sync ./dist s3://basal.odone.io
aws cloudfront create-invalidation \
    --distribution-id ECZT632T2YOYL \
    --paths "/*"
