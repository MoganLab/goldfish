#!/bin/sh
set -eu
project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

# Run after warm-bootstrap-cache.sh; mutate only an isolated copy.
version=$(./bin/gf --check-bootstrap-cache)
scheme_version=$(./bin/gf -m liii -e '(gfo-dir)' | sed 's/^"//; s/"$//')
if [ "$version" != "$scheme_version" ]; then
    echo "bootstrap cache: native and Scheme fingerprints disagree" >&2
    exit 1
fi
cache_test_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-cache-recovery.XXXXXX")
trap 'rm -rf "$cache_test_root"' EXIT HUP INT TERM
export GOLDFISH_CACHE_DIR="$cache_test_root"
target=$(./bin/gf --bootstrap-cache-directory)
mkdir -p "$target"
cp -a "$version"/. "$target"/
./bin/gf --check-bootstrap-cache >/dev/null
printf '(gfo 0 (' > "$target/scheme/base.scm-o2.gfo"
if ./bin/gf --check-bootstrap-cache > /dev/null 2>&1; then
    echo "bootstrap cache: accepted a truncated deferred base artifact" >&2
    exit 1
fi
result=$(GOLDFISH_OPT_LEVEL=2 ./bin/gf -m r7rs -e '(+ 20 22)')
if [ "$result" != 42 ]; then
    echo "bootstrap cache: source recovery returned $result" >&2
    exit 1
fi
./bin/gf --check-bootstrap-cache >/dev/null
echo "bootstrap cache: fingerprint parity and automatic corruption recovery passed"
