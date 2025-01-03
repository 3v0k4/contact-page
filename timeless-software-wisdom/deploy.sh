./pdf.sh

# rsync \
#   --rsync-path="sudo rsync" \
#   -a \
#   timeless-software-wisdom.pdf \
#   riccardo@odone.io:/usr/share/nginx/html/odone.io/

aws s3 cp timeless-software-wisdom.pdf s3://odone.io
aws cloudfront create-invalidation \
    --distribution-id EHEV8XRM85R9X \
    --paths "/timeless-software-wisdom.pdf"
