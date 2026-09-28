#!/bin/sh
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

followups=tests/NATIVE-FOLLOWUPS.tsv
corpus=tests/native-workflow.manifest

tmp_dir=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-native-manifest.XXXXXX")
trap 'rm -rf "$tmp_dir"' EXIT HUP INT TERM

awk -F '\t' '
    /^#/ { next }
    NF != 4 || $1 == "" || $2 == "" || $3 == "" || $4 == "" {
        printf "invalid follow-up row %d\n", NR > "/dev/stderr"; bad=1; next
    }
    $3 != "defer" && $3 != "exclude" {
        printf "invalid status at row %d: %s\n", NR, $3 > "/dev/stderr"; bad=1
    }
    seen[$1]++ { printf "duplicate path at row %d: %s\n", NR, $1 > "/dev/stderr"; bad=1 }
    { print $1 }
    END { if (bad) exit 1 }
' "$followups" > "$tmp_dir/followups"

while IFS= read -r path; do
    [ -f "$path" ] || { echo "follow-up test not found: $path" >&2; exit 1; }
done < "$tmp_dir/followups"

awk '!/^#/ && NF { if (NF != 1) { print "invalid corpus row " NR > "/dev/stderr"; bad=1 }; print $1 } END { exit bad }' "$corpus" > "$tmp_dir/corpus"
[ -s "$tmp_dir/corpus" ] || { echo "native workflow corpus is empty" >&2; exit 1; }
if [ -n "$(sort "$tmp_dir/corpus" | uniq -d)" ]; then
    echo "duplicate corpus path" >&2
    exit 1
fi
while IFS= read -r path; do
    [ -f "$path" ] || { echo "corpus test not found: $path" >&2; exit 1; }
done < "$tmp_dir/corpus"
grep -qx 'tests/native/native-workflow.scm' "$tmp_dir/corpus" || {
    echo "cross-library workflow missing from corpus" >&2; exit 1;
}

echo "Native manifests valid: $(wc -l < "$tmp_dir/corpus" | tr -d ' ') corpus files, $(wc -l < "$tmp_dir/followups" | tr -d ' ') follow-ups"
