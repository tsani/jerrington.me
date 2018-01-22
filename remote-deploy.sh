#!/bin/bash

set -e

ssh j 'ssh blog bash' <<'EOF'
export PATH=$HOME/.local/bin:$PATH
export LANG=en_US.UTF-8
set -e
cd jerrington.me
git fetch
git reset --hard origin/master
git submodule update --init school
make
EOF

echo '>>> DEPLOY COMPLETE'
