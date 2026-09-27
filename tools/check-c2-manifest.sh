#!/bin/sh
# Validate the inputs that define a C2 parity sweep.
set -eu
project=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project"

manifest=${C2_MANIFEST:-tests/float-free.manifest}
skip_file=${C2_SKIP_MANIFEST:-tests/c2-skip.tsv}

test -f "$manifest" || { echo "C2 manifest not found: $manifest" >&2; exit 1; }
test -f "$skip_file" || { echo "C2 skip manifest not found: $skip_file" >&2; exit 1; }

awk '
    NF != 1 { printf "invalid manifest row %d: expected one path\n", NR > "/dev/stderr"; bad=1 }
    seen[$1]++ { printf "duplicate manifest path at row %d: %s\n", NR, $1 > "/dev/stderr"; bad=1 }
    END { if (NR == 0) { print "empty C2 manifest" > "/dev/stderr"; bad=1 }; exit bad }
' "$manifest"

awk -F '\t' '
    /^#/ { next }
    NF != 3 || $1 == "" || $2 == "" || $3 == "" {
        printf "invalid skip row %d: expected path<TAB>bucket<TAB>reason\n", NR > "/dev/stderr"; bad=1; next
    }
    seen[$1]++ { printf "duplicate skip path at row %d: %s\n", NR, $1 > "/dev/stderr"; bad=1 }
    { print $1 }
    END { if (bad) exit 1 }
' "$skip_file" > "${TMPDIR:-/tmp}/c2-skip-paths.$$"

cleanup() { rm -f "${TMPDIR:-/tmp}/c2-skip-paths.$$" "${TMPDIR:-/tmp}/c2-manifest-paths.$$"; }
trap cleanup EXIT HUP INT TERM
sort -u "$manifest" > "${TMPDIR:-/tmp}/c2-manifest-paths.$$"
while IFS= read -r path; do
    grep -qxF "$path" "${TMPDIR:-/tmp}/c2-manifest-paths.$$" || {
        echo "skip path is outside C2 manifest: $path" >&2; exit 1;
    }
    test -f "$path" || { echo "skip path does not exist: $path" >&2; exit 1; }
done < "${TMPDIR:-/tmp}/c2-skip-paths.$$"

echo "C2 manifest valid: $(wc -l < "$manifest" | tr -d ' ') in-scope files, $(wc -l < "${TMPDIR:-/tmp}/c2-skip-paths.$$") bucketed files"
