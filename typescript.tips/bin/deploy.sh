#!/bin/bash

set -e

rm -rf dist/
mkdir dist/
npm install
npx ts-node bin/sitemap.ts
cp -rf public/* dist/
npx tailwindcss -i src/input.css -o dist/output.css --minify
npx ts-node bin/build.ts
rsync \
  --rsync-path="sudo rsync" \
  -a \
  dist/ riccardo@odone.io:/usr/share/nginx/html/typescript.tips/
  # typescript.tips
aws s3 sync ./dist s3://typescript.tips # aws2.typescript.tips
