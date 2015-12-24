#!/bin/bash

set -e

cabal run rebuild
rsync -Pr _site/ /srv/http/blog
