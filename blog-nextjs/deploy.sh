#!/usr/bin/env bash
set -euxo pipefail

yarn build
aws s3 sync ./out s3://odone.io
aws cloudfront create-invalidation \
    --distribution-id EHEV8XRM85R9X \
    --paths "/*"
