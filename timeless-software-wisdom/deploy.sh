./pdf.sh

rsync \
  --rsync-path="sudo rsync" \
  -a \
  timeless-software-wisdom.pdf \
  riccardo@odone.io:/usr/share/nginx/html/odone.io/
