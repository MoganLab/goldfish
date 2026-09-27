#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-c3-manifest.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT HUP INT TERM

awk -F '\t' '!/^#/ && NF { if (NF != 5 || $1 == "" || $2 == "" || $3 == "" || $4 == "" || $5 == "") { print "invalid disposition row: " $0 > "/dev/stderr"; bad=1 } if ($3 != "migrate-before-R4" && $3 != "exclude" && $3 != "defer") { print "invalid disposition: " $0 > "/dev/stderr"; bad=1 } } END { exit bad }' \
    tests/C3-SKIP-DISPOSITIONS.tsv
awk -F '\t' '!/^#/ && NF { print $1 }' tests/C3-SKIP-DISPOSITIONS.tsv | sort > "$tmp_dir/dispositions"
awk -F '\t' '!/^#/ && NF { print $1 }' tests/c2-skip.tsv | sort > "$tmp_dir/skips"

if [ -n "$(uniq -d "$tmp_dir/dispositions")" ]; then
    echo "check-c3-manifest: duplicate skip disposition path" >&2
    exit 1
fi
if ! cmp -s "$tmp_dir/skips" "$tmp_dir/dispositions"; then
    echo "check-c3-manifest: dispositions do not cover tests/c2-skip.tsv exactly" >&2
    diff -u "$tmp_dir/skips" "$tmp_dir/dispositions" >&2 || true
    exit 1
fi

awk '!/^#/ && NF { print $1 }' tests/c3-native.manifest > "$tmp_dir/corpus"
if [ -s "$tmp_dir/corpus" ] && [ -n "$(sort "$tmp_dir/corpus" | uniq -d)" ]; then
    echo "check-c3-manifest: duplicate corpus path" >&2
    exit 1
fi
while IFS= read -r path; do
    [ -f "$path" ] || { echo "check-c3-manifest: missing $path" >&2; exit 1; }
done < "$tmp_dir/corpus"
if ! grep -qx 'tests/c3/native-workflow-test.scm' "$tmp_dir/corpus"; then
    echo "check-c3-manifest: cross-library workflow missing from corpus" >&2
    exit 1
fi

echo "C3 manifests valid: $(wc -l < "$tmp_dir/corpus" | tr -d ' ') corpus files, $(wc -l < "$tmp_dir/dispositions" | tr -d ' ') skip dispositions"
