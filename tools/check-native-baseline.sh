#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
root=tests/native-baseline
work=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-baseline-check.XXXXXX")
trap 'rm -rf "$work"' EXIT HUP INT TERM
awk -F '\t' '
  NR == 1 {if ($0 != "path\tverdict\tcoverage\tevidence") exit 1; next}
  NF != 4 || seen[$1]++ || $2 !~ /^(pass|fail)$/ || $3 !~ /^(executed|http-opt-in)$/ {
    print "invalid baseline result: " NR > "/dev/stderr"; bad=1
  }
  {print $1}
  END {exit bad}
' "$root/results.tsv" > "$work/unsorted"
LC_ALL=C sort "$work/unsorted" > "$work/results"
LC_ALL=C sort "$root/discovered.manifest" > "$work/discovered"
diff -u "$work/discovered" "$work/results"
revision=$(awk -F '\t' '$1 == "source_commit" {print $2}' "$root/metadata.tsv")
git cat-file -e "$revision^{commit}"
git ls-tree -r --name-only "$revision" -- tests > "$work/tracked"
awk '/-test\.scm$/' "$work/tracked" | LC_ALL=C sort > "$work/revision-files"
diff -u "$work/revision-files" "$work/discovered"
for pair in 'full-run.log transcript_sha256' 'results.tsv results_sha256' 'discovered.manifest discovery_sha256'; do
    set -- $pair
    actual=$(sha256sum "$root/$1" | cut -d ' ' -f1)
    expected=$(awk -F '\t' -v key="$2" '$1 == key {print $2}' "$root/metadata.tsv")
    [ "$actual" = "$expected" ] || { echo "baseline checksum mismatch: $1" >&2; exit 1; }
done
for pair in 'migration-recheck.log migration_transcript_sha256' 'migration-tested.patch migration_patch_sha256'; do
    set -- $pair
    actual=$(sha256sum "$root/$1" | cut -d ' ' -f1)
    expected=$(awk -F '\t' -v key="$2" '$1 == key {print $2}' "$root/metadata.tsv")
    [ "$actual" = "$expected" ] || { echo "baseline recheck checksum mismatch: $1" >&2; exit 1; }
done
awk -F '\t' '
  NR==FNR {
    if (FNR > 1) {
      if ($4 !~ /^tests\/native-baseline\/full-run.log:[0-9]+$/) bad=1
      split($4, parts, ":"); expected[parts[2]]="  " $1 " ... " toupper($2)
      count++; if ($2 == "fail") failed++
    }
    next
  }
  FNR in expected {if ($0 != expected[FNR]) bad=1; found++}
  /^  Total:  / {if (substr($0, 11)+0 != count) bad=1; total_seen=1}
  /^  Passed: / {if (substr($0, 11)+0 != count-failed) bad=1; passed_seen=1}
  /^  Failed: / {if (substr($0, 11)+0 != failed) bad=1; failed_seen=1}
  END {if (bad || found != count || !total_seen || !passed_seen || (failed && !failed_seen)) {
    print "baseline transcript and results disagree" > "/dev/stderr"; exit 1
  }}
' "$root/results.tsv" "$root/full-run.log"
awk -F '\t' '
  NR==FNR {
    if (FNR > 1) {result[$1]=$2; coverage[$1]=$3; evidence[$1]=$4}
    next
  }
  /^#/ || $1 == "path" {next}
  NF != 5 || seen[$1]++ || !($1 in result) || $3 !~ /^(resolved|fixed|defer|exclude|guarded)$/ || $4 == "" || $5 == "" {
    print "invalid baseline disposition: " FNR > "/dev/stderr"; bad=1
  }
  $3 == "resolved" && result[$1] != "pass" {bad=1}
  $3 == "fixed" && result[$1] != "fail" {bad=1}
  $3 == "guarded" && (coverage[$1] != "http-opt-in" || result[$1] != "pass") {bad=1}
  $4 != evidence[$1] {bad=1}
  END {
    for (path in result) if ((result[path] == "fail" || coverage[path] == "http-opt-in") && !seen[path]) {
      print "missing failure/guard disposition: " path > "/dev/stderr"; bad=1
    }
    exit bad
  }
' "$root/results.tsv" "$root/dispositions.tsv"
awk -F '\t' '
  NR==FNR {if (!/^#/ && $1 != "path") seen[$1]=1; next}
  /^#/ || $1 == "path" {next}
  !seen[$1] {print "unreviewed earlier disposition: " $1 > "/dev/stderr"; bad=1}
  END {exit bad}
' "$root/dispositions.tsv" "$root/historical-dispositions.tsv" tests/NATIVE-FOLLOWUPS.tsv
awk -F '\t' '
  NR==FNR {
    if (FNR > 1) {
      if (NF != 3 || seen[$1]++ || $2 !~ /^[0-9]+$/) bad=1
      code[$1]=$2; evidence[$1]=$3
    }
    next
  }
  $3 == "fixed" {
    if (!($1 in code) || code[$1] != 0 || evidence[$1] !~ /^tests\/native-baseline\/[^:]+:[0-9]+$/) {
      print "missing passing recheck for fixed baseline failure: " $1 > "/dev/stderr"; bad=1
    }
  }
  END {exit bad}
' "$root/rechecks.tsv" "$root/dispositions.tsv"
tab=$(printf '\t')
while IFS="$tab" read -r path code evidence; do
    [ "$code" = 0 ] || continue
    log=${evidence%:*}
    line=${evidence##*:}
    awk -v line="$line" -v expected="  $path ... PASS" '
      NR == line {found=1; if ($0 != expected) bad=1}
      END {exit bad || !found}
    ' "$log"
done < "$root/rechecks.tsv"
echo "Native baseline discovery, failures, guards and historical dispositions are accounted for"
