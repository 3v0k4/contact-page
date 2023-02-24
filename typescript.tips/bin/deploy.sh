#!/bin/bash

set -e

npm install
npm run build
aws s3 sync ./dist s3://typescript.tips
aws cloudfront create-invalidation \
    --distribution-id E2K2FTV79X2003 \
    --paths "/*"
