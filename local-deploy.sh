#!/bin/bash

# Deploys a locally built site, as opposed to remote-deploy, which
# builds the site on the deployment target.

set -e

rsync -r _site/ j:/srv/http/blog/
ssh j 'chmod -R +r /srv/http/blog'
