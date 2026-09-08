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

kernel_sources="goldfish/expander/kernel/*.scm goldfish/expander/kernel.scm"
artifact=goldfish/expander/kernel-combined.scm

# A paren imbalance surfaces as a confusing failure deeper in the expansion;
# scan first so the reader damage is reported by file and line.
./bin/gf -m liii tools/scan-parens.scm $kernel_sources

rm -rf "${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"
out=$(./bin/gf goldfish/expander/build-combined.scm) || {
    echo "build-kernel: expansion failed -- the artifact was not updated" >&2
    exit 1
}
echo "$out"

# The writer's note is the only completion signal: without it the old
# artifact stays in place and everything downstream stays green on stale
# code -- which is exactly the failure mode this guard exists for.
echo "$out" | grep -q "wrote $artifact" || {
    echo "build-kernel: no 'wrote $artifact' marker in build output" >&2
    exit 1
}

for f in $kernel_sources; do
    if [ "$f" -nt "$artifact" ]; then
        echo "build-kernel: $f is newer than the artifact" >&2
        exit 1
    fi
done
