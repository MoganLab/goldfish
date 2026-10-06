#!/bin/sh
# Build an optimized, symbol-resolvable profiling binary at bin/gf-prof
# without disturbing the release bin/gf.
#
#   tools/build-prof.sh
#   tools/prof.sh bin/gf-prof bench/micro/reader.scm
#
# releasedbg is optimized but the toolchain strips it; it also emits the
# debug file bin/gf.sym and a .gnu_debuglink in the stripped binary, which
# is what lets perf resolve symbols.  We copy the binary to bin/gf-prof
# (its debuglink still names gf.sym, which sits beside it) and restore the
# release bin/gf.
set -eu
cd "$(dirname "$0")/.."

[ -x bin/gf ] || { echo "build-prof: bin/gf missing -- run 'xmake b' first" >&2; exit 1; }
if [ -x bin/gf-prof ] && [ bin/gf-prof -nt bin/gf ] && [ -f bin/gf.sym ]; then
    echo "build-prof: bin/gf-prof is up to date"; exit 0
fi

cp -f bin/gf bin/gf.release
rm -f bin/gf
restore() {
    xmake f -m release >/dev/null 2>&1 || true
    cp -f bin/gf.release bin/gf
    rm -f bin/gf.release
}
trap restore EXIT

xmake f -m releasedbg >/dev/null
xmake b gf-native >/dev/null
cp -f bin/gf bin/gf-prof
echo "build-prof: wrote bin/gf-prof (+ bin/gf.sym; optimized, symbols via debuglink)"
