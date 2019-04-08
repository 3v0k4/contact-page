!#/usr/bin/env bash
set -euxo pipefail

cd blog
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
cd -
