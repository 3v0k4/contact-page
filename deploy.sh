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

rsync \
  --rsync-path="sudo rsync" \
  -a \
  curves riccardo@odone.io:/usr/share/nginx/html/odone.io/

cd xpath
yarn compile
sed -i '' -e "s/VERSION/$(date +%s)/g" public/index.html
rsync \
  --rsync-path="sudo rsync" \
  -a \
  public/ riccardo@odone.io:/usr/share/nginx/html/odone.io/xpath/
git checkout public/index.html
cd -

cd timeless-software-wisdom
./pdf.sh
rsync \
  --rsync-path="sudo rsync" \
  -a \
  timeless-software-wisdom.pdf \
  riccardo@odone.io:/usr/share/nginx/html/odone.io/
cd -

cd typescript.tips
./deploy.sh
cd -
