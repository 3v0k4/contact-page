#!/bin/bash

set -e

rm -rf dist/
mkdir dist/
./sitemap.sh
npm install
npx tailwindcss -i input.css -o dist/output.css --minify
cp -rf public/* dist/
sed -i '' -e "s/VERSION/$(date +%s)/g" dist/index.html
./tips.sh dist/index.html
./tips.sh dist/template.html
./build.sh
rsync \
  --rsync-path="sudo rsync" \
  -a \
  dist/ riccardo@odone.io:/usr/share/nginx/html/typescript.tips/
