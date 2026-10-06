#!/bin/sh
# Build an unstripped profiling binary at bin/gf-prof without disturbing the
# release bin/gf.
#
#   tools/build-prof.sh
#   tools/prof.sh bin/gf-prof -e '(...)'   # or GOLDFISH_PROF_BIN=bin/gf-prof
#
# Debug objects land in the shared build dir; the next 'xmake b' returns to
# the release configuration.  The release binary is saved and restored.
set -eu
cd "$(dirname "$0")/.."

[ -x bin/gf ] || { echo "build-prof: bin/gf missing -- run 'xmake b' first" >&2; exit 1; }
[ -x bin/gf-prof ] && [ bin/gf-prof -nt bin/gf ] && {
    echo "build-prof: bin/gf-prof is up to date"; exit 0; }

saved=bin/gf.release
cp -f bin/gf "$saved"
rm -f bin/gf
restore() {
    xmake f -m release >/dev/null 2>&1 || true
    cp -f "$saved" bin/gf
    rm -f "$saved"
}
trap restore EXIT

xmake f -m debug --cxflags="-O2" --cxxflags="-O2" >/dev/null
xmake b gf-native >/dev/null
cp -f bin/gf bin/gf-prof
echo "build-prof: wrote bin/gf-prof (optimized, unstripped)"
