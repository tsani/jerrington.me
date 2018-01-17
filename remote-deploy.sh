#!/bin/bash

set -e

ssh -t j 'ssh blog bash' <<'EOF'
set -e
cd jerrington.me
git fetch
git reset --hard origin/master
make
EOF

echo '>>> DEPLOY COMPLETE'
