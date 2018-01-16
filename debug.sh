#!/bin/bash

set -e

stack exec site rebuild
(sleep 1 && open "http://localhost:8080") &!
(cd _site ; python -m http.server 8080)
