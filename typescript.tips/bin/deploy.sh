#!/bin/bash

set -e

rm -rf dist/
mkdir dist/
npm install
npx ts-node bin/sitemap.ts
cp -rf public/* dist/
npx tailwindcss -i src/input.css -o dist/output.css --minify
npx ts-node bin/build.ts
aws s3 sync ./dist s3://typescript.tips
aws cloudfront create-invalidation \
    --distribution-id E2K2FTV79X2003 \
    --paths "/*"
