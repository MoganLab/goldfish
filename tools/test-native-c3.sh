#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

manifest=tests/c3-native.manifest
files=$(sed '/^[[:space:]]*#/d; /^[[:space:]]*$/d' "$manifest")
if [ -z "$files" ]; then
    echo "test-native-c3: empty corpus in $manifest" >&2
    exit 1
fi

sh tools/check-c3-manifest.sh

# The base gate checks clean-cache, source-bootstrap, and warm-cache paths.
sh tools/test-native.sh

eval_result=$(./bin/gf -m r7rs -e '(+ 20 22)')
if [ "$eval_result" != "42" ]; then
    echo "test-native-c3: -e returned $eval_result, expected 42" >&2
    exit 1
fi

file_result=$(./bin/gf -m r7rs tests/runtime/fixtures/native-cli-regression.scm)
if [ "$file_result" != "42" ]; then
    echo "test-native-c3: file execution returned $file_result, expected 42" >&2
    exit 1
fi

repl_result=$(printf '(define c3-repl-state 40)\n(+ c3-repl-state 2)\n' \
    | ./bin/gf -m r7rs)
case "$repl_result" in
    *42*) ;;
    *)
        echo "test-native-c3: REPL smoke did not produce 42" >&2
        exit 1
        ;;
esac
echo "native CLI smoke passed: -e, file execution, stateful REPL"

# Reuse one native boot while keeping each test's process state isolated.
# shellcheck disable=SC2086
./bin/gf -m liii --each-file $files

# Exercise the integrated cross-library program from an isolated empty cache.
# The CLI's `load` handler uses the C++ source loader when Scheme `load` has
# not yet been installed by cold bootstrap.
cold_cache=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-native-c3-cold.XXXXXX")
trap 'rm -rf "$cold_cache"' EXIT HUP INT TERM
mkdir -p "$cold_cache/ccache"
GOLDFISH_CACHE_DIR="$cold_cache/ccache" \
    ./bin/gf -m liii load tests/c3/native-workflow.scm
