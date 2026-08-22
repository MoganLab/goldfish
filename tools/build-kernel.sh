#!/bin/sh
# Rebuild the expander kernel artifact (goldfish/expander/kernel-combined.scm)
# from source via a cold-cache from-artifact bootstrap.
#
# The cache wipe is not optional: warm-cache rebuilds may drift in internal
# gensym numbering (lib-layer macro cache behavior), so only cold builds are
# byte-reproducible -- see LAYER.md 演进债 and devel/200_82.md.
set -eu
cd "$(dirname "$0")/.."

if [ ! -x bin/gf ]; then
    echo "build-kernel: bin/gf not found -- run 'xmake b' first" >&2
    exit 1
fi

rm -rf "${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"
EXPANDER_BOOT=from-artifact ./bin/gf goldfish/expander/build-combined.scm
