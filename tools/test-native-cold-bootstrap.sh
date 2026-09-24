#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

run_cold() {
    cache_dir=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-native-cold.XXXXXX")
    trap 'rm -rf "$cache_dir"' EXIT HUP INT TERM
    mkdir -p "$cache_dir/goldfish/ccache"
    GOLDFISH_CACHE_DIR="$cache_dir/goldfish/ccache" \
        GOLDFISH_OPT_LEVEL=0 \
        "$project_dir/bin/gf-native" "$@"
    trap - EXIT HUP INT TERM
    rm -rf "$cache_dir"
}

result=$(run_cold -m r7rs -e '(+ 20 22)' 2>/dev/null)
test "$result" = 42

# `load` (not `test`): the dispatcher now claims `test` for the project
# tool, whose runner needs a warm cache -- this suite runs COLD on purpose.
run_cold -m r7rs load \
    "$project_dir/tests/runtime/fixtures/native-source-bootstrap.scm" \
    "$project_dir/tests/runtime/fixtures/native-cli-regression.scm" \
    "$project_dir/tests/runtime/fixtures/native-eval-environment.scm" \
    >/dev/null
