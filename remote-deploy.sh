#!/bin/bash

set -e

ssh -t j 'ssh blog bash' <<'EOF'
export PATH=$HOME/.local/bin:$PATH
export LANG=en_US.UTF-8
set -e
cd jerrington.me
git fetch
git reset --hard origin/master
make
EOF

echo '>>> DEPLOY COMPLETE'
