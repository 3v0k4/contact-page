!#/usr/bin/env bash
set -euxo pipefail

cd blog
sed -i '' -e "s/VERSION/$(date +%s)/g" templates/default.html
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
