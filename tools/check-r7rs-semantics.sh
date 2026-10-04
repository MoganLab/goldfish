#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
ledger=R7RS-SEMANTIC-AUDIT.tsv
case "${1:-}" in ''|--ledger-only) ;; *) exit 2 ;; esac
awk -F '\t' '
  /^#/ || $1 == "id" { next }
  NF != 7 { print "invalid semantic audit row: " NR > "/dev/stderr"; bad=1 }
  seen[$1]++ { print "duplicate audit id: " $1 > "/dev/stderr"; bad=1 }
  $3 !~ /^(pass|gap)$/ { print "invalid observation: " $1 > "/dev/stderr"; bad=1 }
  $4 !~ /^tests\/r7rs\/audit\/[^/]+\.scm$/ { print "probe must be a file: " $1 > "/dev/stderr"; bad=1 }
  $5 == "" || $6 == "" || $7 !~ /^https:\/\/standards.scheme.org\// {
    print "missing obligation, boundary or standard source: " $1 > "/dev/stderr"; bad=1
  }
  END { if (bad || !length(seen)) exit 1 }
' "$ledger"
check_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-semantic-check.XXXXXX")
trap 'rm -rf "$check_root"' EXIT HUP INT TERM
files=$(sed '/^[[:space:]]*#/d; /^[[:space:]]*$/d' tests/r7rs/semantic-audit.manifest)
: > "$check_root/probes"
for file in $files; do
    [ -f "$file" ] || { echo "missing semantic probe: $file" >&2; exit 1; }
    sed -n "s/^(audit-check '\([^ ]*\).*/\1/p" "$file" >> "$check_root/probes"
done
sed -n 's/^  ((scheme \([^)]*\)).*/exports.\1/p' tests/r7rs/fixtures/standard-exports.scm >> "$check_root/probes"
sort "$check_root/probes" > "$check_root/probe-ids"
awk -F '\t' '!/^#/ && $1 != "id" {print $1}' "$ledger" | sort > "$check_root/ledger-ids"
diff -u "$check_root/probe-ids" "$check_root/ledger-ids"
awk -F '\t' '
  NR==FNR { if (!/^#/ && $1 != "clause") {clauses[$1]=$3; evidence[$1]=$4}; next }
  /^#/ || $1 == "id" {next}
  { n=split($2, required, ","); for (i=1;i<=n;i++) if (!clauses[required[i]]) {
      print "unknown audit clause: " $1 " / " required[i] > "/dev/stderr"; bad=1
    } else {
      if ($3 == "gap" && clauses[required[i]] != "partial") {
        print "known gap requires partial clause: " $1 > "/dev/stderr"; bad=1
      }
      if (index("," evidence[required[i]] ",", "," $4 ",") == 0) {
        print "clause lacks direct probe evidence: " $1 > "/dev/stderr"; bad=1
      }
    } }
  END {exit bad}
' R7RS-COMPATIBILITY.tsv "$ledger"
if [ "${1:-}" = --ledger-only ]; then
    echo "R7RS semantic ledger and probe IDs are valid"
    exit 0
fi
awk -F '\t' '
  /^#/ || $1 == "id" {next}
  NF != 4 || seen[$1]++ || $2 !~ /^(pass|gap)$/ || $3 !~ /^(pass|gap)$/ || $4 == "" {bad=1}
  END {if (bad) {print "invalid semantic results snapshot" > "/dev/stderr"; exit 1}}
' tests/r7rs/semantic-results.tsv
awk -F '\t' '!/^#/ && $1 != "id" {print $1 "\t" $3}' "$ledger" | sort > "$check_root/expected"
awk -F '\t' '!/^#/ && $1 != "id" {print $1 "\t" $2}' tests/r7rs/semantic-results.tsv | sort > "$check_root/cold"
awk -F '\t' '!/^#/ && $1 != "id" {print $1 "\t" $3}' tests/r7rs/semantic-results.tsv | sort > "$check_root/warm"
diff -u "$check_root/expected" "$check_root/cold"
diff -u "$check_root/expected" "$check_root/warm"
echo "R7RS semantic ledger, probe IDs and recorded cold/warm observations are valid"
