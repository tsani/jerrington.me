#!/bin/bash

set -e

cabal run rebuild
(sleep 1 && exec $BROWSER "http://localhost:8080" > /dev/null 2>&1) &!
(cd _site ; python -m http.server 8080)
