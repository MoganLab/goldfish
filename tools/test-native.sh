#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)

# Native regression entry point.  Scheme test files that exercise the native
# bootstrap belong here; the regular `gf test` command remains the host/s7
# suite for the rest of the repository.
xmake build native-reader-test
"$project_dir/bin/native-reader-test"
sh "$project_dir/tools/test-native-cold-bootstrap.sh"
