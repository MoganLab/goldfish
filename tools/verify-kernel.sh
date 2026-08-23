#!/bin/sh
# Reproduction guard for the expander kernel artifact (LAYER.md 演进债
# 「产物再生产校验」).  Verifies, with two cold-cache rebuilds:
#   1. fixpoint -- the two rebuilds are structurally identical to each other;
#   2. reproduction -- the rebuilt artifact is structurally identical to the
#      committed goldfish/expander/kernel-combined.scm.
# Comparison runs through tools/canonicalize.scm: the global fresh counter
# shifts names like `rtd~493` whenever the boot chain changes how many it
# consumed, and gensym ids carry no meaning, so those suffixes are blinded
# before the byte comparison.
set -eu
cd "$(dirname "$0")/.."

if [ ! -x bin/gf ]; then
    echo "verify-kernel: bin/gf not found -- run 'xmake b' first" >&2
    exit 1
fi

artifact=goldfish/expander/kernel-combined.scm
committed=$(mktemp)
built1=$(mktemp)
built2=$(mktemp)
trap 'rm -f "$committed" "$built1" "$built2"' EXIT

cp "$artifact" "$committed"

rm -rf "${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"
./bin/gf goldfish/expander/build-combined.scm >/dev/null
cp "$artifact" "$built1"

rm -rf "${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"
./bin/gf goldfish/expander/build-combined.scm >/dev/null
cp "$artifact" "$built2"

for f in "$committed" "$built1" "$built2"; do
    ./bin/gf -m liii tools/canonicalize.scm "$f" > "$f.c"
done

if ! cmp -s "$built1.c" "$built2.c"; then
    echo "verify-kernel: FAILED -- cold builds disagree; kernel bootstrap has no fixpoint" >&2
    exit 1
fi

if ! cmp -s "$committed.c" "$built1.c"; then
    echo "verify-kernel: FAILED -- committed artifact differs from regenerated output" >&2
    echo "  (kernel sources changed without rebuilding? run: xmake kernel)" >&2
    exit 1
fi

echo "verify-kernel: OK -- artifact reproduces structurally across cold builds"
