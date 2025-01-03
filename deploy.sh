#!/usr/bin/env bash
set -euxo pipefail

if [[ ! -z $(git status -s) ]]
then
  echo "tree is dirty, please commit changes before running this"
  exit 1
fi

cd blog-nextjs
./deploy.sh
cd -

cd curves
./deploy.sh
cd -

cd xpath
./deploy.sh
cd -

cd timeless-software-wisdom
./deploy.sh
cd -
