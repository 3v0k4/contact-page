---
title: Rewriting to Haskell–Project Setup
description: Setting up Servant and CI for Stream
author: Riccardo
tags:
  - FunctionalProgramming
  - Haskell
  - Servant
---

This is part of a series:

- [Rewriting to Haskell–Intro](https://odone.io/posts/2020-02-26-rewriting-haskell-intro.html)

---

We decided to go with [Servant](https://www.servant.dev/) to rewrite the current Rails API for Stream. We don't really have a strong reason for that. We just like it and we believe it will allow us to do some cool stuff down the line!

Also, we picked [Stack](https://docs.haskellstack.org/en/stable/README/) because it's what we are used to and seems to be less cryptic than Cabal.

We created the project with the following command:

```bash
stack new haskell servant
#     ^ Create a new project..
#         ^ ..in a new haskell/ folder..
#                 ^ ..using the Servant template.
```

The Servant template creates a dummy application with an hardcoded endpoint and some tests that allow to start playing with code right away.

## CI

Stream is already using CircleCI, so we opened the [Haskell languge guide](https://circleci.com/docs/2.0/language-haskell/) and started copy / pasting like there was no tomorrow.

We ended up with the following config.yml file that supports Rails, Elm and Haskell. See inlined comments for more info.

```yml
---
version: 2.1

commands:
  install_dependencies:
# ^ Define a reusable command (see invokations below) to install the dependencies needed for Rails and Elm.
    steps:
      - restore_cache:
          name: Restore bundle cache
          key: stream-{{ checksum "Gemfile.lock" }}

      - restore_cache:
          name: Restore yarn cache
          key: stream-yarn-{{ checksum "yarn.lock" }}

      - run: bundle install --path vendor/bundle

      - run: yarn install

      - save_cache:
          name: Store bundle cache
          key: stream-{{ checksum "Gemfile.lock" }}
          paths:
            - vendor/bundle

      - save_cache:
          name: Store yarn cache
          key: stream-yarn-{{ checksum "yarn.lock" }}
          paths:
            - ~/.cache/yarn/v1
            - node_modules

jobs:
  build:
# ^ Test Rails and Elm code.

    working_directory: ~/stream
    docker:
      - image: circleci/ruby:2.4.1-node-browsers
        environment:
          PGHOST: 127.0.0.1
          PGUSER: stream
          RAILS_ENV: test
      - image: circleci/postgres:9.5-alpine
        environment:
          POSTGRES_USER: stream
          POSTGRES_DB: stream_test
          POSTGRES_PASSWORD: ""

    steps:
      - checkout

      - install_dependencies

      # sysconfcpus is a hack for elm-make
      # https://github.com/elm-lang/elm-compiler/issues/1473
      - run: |
          git clone https://github.com/obmarg/libsysconfcpus.git ~/libsysconfcpus
          cd ~/libsysconfcpus
          ./configure
          make
          sudo make install
          cd -

      - run: yarn test

      - run: yarn run elm-format --validate app/javascript/

      - run: sysconfcpus --num 2 yarn run elm-test app/javascript/tests

      - run: dockerize -wait tcp://localhost:5432 -timeout 1m

      - run: cp config/database.yml.example config/database.yml

      - run: bin/rails db:setup

      - run: sysconfcpus --num 2 bin/rspec spec

      - run: bundle exec bundle audit check --update

      - run: bundle exec brakeman --ensure-latest -A -5

      - run: bundle exec bundle outdated || true

  build_haskell:
# ^ Test Haskell code.

    working_directory: ~/stream/haskell

    docker:
      - image: fpco/stack-build:lts

    steps:
      - checkout:
          path: ~/stream

      - restore_cache:
          name: Restore Cached Dependencies
          keys:
            - stream-haskell-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
            - stream-haskell-{{ checksum "stack.yaml" }}

      - run:
          name: Resolve/Update Dependencies
          command: stack --no-terminal setup

      - run:
          name: Run tests
          command: stack --no-terminal test

      - run:
          name: Install executable
          command: stack --no-terminal install

      - save_cache:
          name: Cache Dependencies
          key: stream-haskell-{{ checksum "stack.yaml" }}-{{ checksum "package.yaml" }}
          paths:
            - "/root/.stack"
            - ".stack-work"

      - store_artifacts:
          path: ~/.local/bin/haskell-exe
          destination: haskell-exe

  deploy:
# ^ Deploy the Rails and Elm application. We will see in a later post how to deploy the Servant application.

    working_directory: ~/stream

    docker:
      - image: circleci/ruby:2.4.1-node-browsers

    steps:
      - checkout

      - install_dependencies

      - run : |
            if [ "${CIRCLE_BRANCH}" == "master" ]; then
              bundle exec cap staging deploy
            elif [ "${CIRCLE_BRANCH}" == "production" ]; then
              bundle exec cap production deploy
            else
              echo "${CIRCLE_BRANCH} is a feature branch so no deploy"
            fi

workflows:
  version: 2
  build_and_deploy:
    jobs:
      - build
      - build_haskell
      - deploy:
          requires:
            - build
            - build_haskell
```
