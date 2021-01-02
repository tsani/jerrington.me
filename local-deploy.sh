#!/bin/bash

# Deploys a locally built site, as opposed to remote-deploy, which
# builds the site on the deployment target.

ssh j bash <<EOF
set -e
rm -rf /tmp/blog
mkdir -p /tmp/blog
EOF

rsync -r _site/ j:/tmp/blog/

ssh j bash <<EOF
rsync -r /tmp/blog/ blog:/srv/http/blog/
EOF
