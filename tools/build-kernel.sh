#!/bin/sh
# Rebuild the expander kernel artifact (goldfish/expander/kernel-combined.scm)
# from source via a cold-cache rebuild (the committed artifact boots the expander).
#
# The cache wipe keeps rebuilds on one code path (lib-layer macro caches can
# alter expansion details); gensym numbering itself is NOT expected to match
# across runs -- tools/verify-kernel.sh compares canonicalized artifacts.
set -eu
cd "$(dirname "$0")/.."

if [ ! -x bin/gf ]; then
    echo "build-kernel: bin/gf not found -- run 'xmake b' first" >&2
    exit 1
fi

rm -rf "${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"
./bin/gf goldfish/expander/build-combined.scm
