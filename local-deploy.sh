#!/bin/bash

# Deploys a locally built site, as opposed to remote-deploy, which
# builds the site on the deployment target.

exec rsync -r _site/ j:/srv/http/blog/
