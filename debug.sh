#!/bin/bash

set -e

cabal run rebuild
(cd _site ; python -m http.server 8080)
