#!/usr/bin/env bash


stack runghc \
  --resolver lts-14.17 \
  --package composition-prelude-2.0.5.0 \
  --package htoml-megaparsec-2.1.0.3 \
  --package frontmatter \
  --package yaml \
  --package bytestring \
  --package aeson \
  --package text \
  --package filepath \
  --package tweet-hs \
  --package optparse-applicative \
  --package directory \
  ./tweet.hs "$@"
