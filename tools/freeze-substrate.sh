#!/bin/sh
# Freeze the native evaluator's lowered-core language contract.
set -e
cd "$(dirname "$0")/.."

echo "== D core-language =="
core_rows () {
  sed -n '/(define core-language/,/core-form?/p' goldfish/core/ir.scm \
    | grep -v -e core-language -e core-form? \
    | grep -oE "^[ ]+'?\\(+[a-z!*+/-]+" \
    | sed -E "s/^[^a-z!*+/-]*//" \
    | sort -u
}
core_rows
echo "count=$(core_rows | wc -l | tr -d ' ')"
