#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

# Native regression entry point.  Scheme test files that exercise the native
# bootstrap belong here. The default `gf test` path now uses the native
# runtime; the explicit `gf-host` binary remains available for host oracles.
xmake build native-reader-test
"$project_dir/bin/native-reader-test"

# The library source test replays a PREBUILT cache, so the cache has to be
# complete before it runs; warm-bootstrap-cache.sh owns that step.
xmake build native-library-source-test
xmake build gf-native
sh "$project_dir/tools/warm-bootstrap-cache.sh"
"$project_dir/bin/native-library-source-test"

sh "$project_dir/tools/test-native-cold-bootstrap.sh"
