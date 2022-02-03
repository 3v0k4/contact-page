#!/usr/bin/env bash
set -euxo pipefail

if [[ ! -z $(git status -s) ]]
then
  echo "tree is dirty, please commit changes before running this"
  exit 1
fi

cd blog
sed -i '' -e "s/VERSION/$(date +%s)/g" templates/default.html
stack build
stack exec site rebuild
rsync \
  --rsync-path="sudo rsync" \
  -a --filter='P _site/' \
  --filter='P _cache/' \
  --filter='P .git/' \
  --filter='P .gitignore' \
  --filter='P .stack-work' \
  --delete-excluded \
  _site/ riccardo@odone.io:/usr/share/nginx/html/
git checkout -- templates/default.html
cd -

rsync \
  --rsync-path="sudo rsync" \
  -a \
  --delete-excluded \
  curves riccardo@odone.io:/usr/share/nginx/html/

cd xpath
yarn compile
sed -i '' -e "s/VERSION/$(date +%s)/g" public/index.html
rsync \
  --rsync-path="sudo rsync" \
  -a \
  --delete-excluded \
  public/ riccardo@odone.io:/usr/share/nginx/html/xpath/
git checkout public/index.html
cd -

rsync \
  --rsync-path="sudo rsync" \
  -a \
  timeless-software-wisdom.pdf \
  riccardo@odone.io:/usr/share/nginx/html/
