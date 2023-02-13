#!/bin/bash

set -e

rm -rf dist/
mkdir dist/
./sitemap.sh
npm install
cp -rf public/* dist/
npx tailwindcss -i input.css -o dist/output.css --minify
./build.sh
rsync \
  --rsync-path="sudo rsync" \
  -a \
  dist/ riccardo@odone.io:/usr/share/nginx/html/typescript.tips/
