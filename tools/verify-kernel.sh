#!/bin/sh
# Reproduction guard for the expander kernel artifact (LAYER.md 演进债
# 「产物再生产校验」).  Verifies, with two cold-cache from-artifact builds:
#   1. fixpoint -- the two rebuilds are byte-identical to each other;
#   2. reproduction -- the rebuilt artifact equals the committed
#      goldfish/expander/kernel-combined.scm (source and artifact in sync).
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
EXPANDER_BOOT=from-artifact ./bin/gf goldfish/expander/build-combined.scm >/dev/null
cp "$artifact" "$built1"

rm -rf "${XDG_CACHE_HOME:-$HOME/.cache}/goldfish/ccache"
EXPANDER_BOOT=from-artifact ./bin/gf goldfish/expander/build-combined.scm >/dev/null
cp "$artifact" "$built2"

if ! cmp -s "$built1" "$built2"; then
    echo "verify-kernel: FAILED -- cold builds disagree; kernel bootstrap has no fixpoint" >&2
    exit 1
fi

if ! cmp -s "$committed" "$built1"; then
    echo "verify-kernel: FAILED -- committed artifact differs from regenerated output" >&2
    echo "  (kernel sources changed without rebuilding? run: xmake kernel)" >&2
    exit 1
fi

echo "verify-kernel: OK -- artifact reproduces byte-for-byte across cold builds"
