npm install
npx tailwindcss -i input.css -o dist/output.css --minify
cp -rf public/* dist/
sed -i '' -e "s/VERSION/$(date +%s)/g" dist/index.html
rsync \
  --rsync-path="sudo rsync" \
  -a \
  dist/ riccardo@odone.io:/usr/share/nginx/html/basal.odone.io/
