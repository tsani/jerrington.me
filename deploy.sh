#!/bin/bash

set -e

make
cabal run rebuild
rsync -Pr _site/ /srv/http/blog
