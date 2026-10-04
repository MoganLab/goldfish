#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"

awk -F '\t' '
  /^#/ { next }
  $1 == "clause" { next }
  NF != 5 { print "invalid matrix row: " NR > "/dev/stderr"; bad=1 }
  seen[$1]++ { print "duplicate clause: " $1 > "/dev/stderr"; bad=1 }
  $3 !~ /^(audited|partial|inventory|unverified)$/ {
    print "invalid status: " $1 > "/dev/stderr"; bad=1
  }
  END {
    n=split("1.1 1.2 1.3.1 1.3.2 1.3.3 1.3.4 1.3.5 2.1 2.2 2.3 2.4 3.1 3.2 3.3 3.4 3.5 4.1.1 4.1.2 4.1.3 4.1.4 4.1.5 4.1.6 4.1.7 4.2.1 4.2.2 4.2.3 4.2.4 4.2.5 4.2.6 4.2.7 4.2.8 4.2.9 4.3.1 4.3.2 4.3.3 5.1 5.2 5.3.1 5.3.2 5.3.3 5.4 5.5 5.6.1 5.6.2 5.7 6.1 6.2.1 6.2.2 6.2.3 6.2.4 6.2.5 6.2.6 6.2.7 6.3 6.4 6.5 6.6 6.7 6.8 6.9 6.10 6.11 6.12 6.13.1 6.13.2 6.13.3 6.14 7.1.1 7.1.2 7.1.3 7.1.4 7.1.5 7.1.6 7.1.7 7.2.1 7.2.2 7.2.3 7.2.4 7.3 A B", required, " ")
    for (i=1; i<=n; i++) if (!seen[required[i]]) {
      print "missing report clause: " required[i] > "/dev/stderr"; bad=1
    }
    if (bad) exit 1
  }
' R7RS-COMPATIBILITY.tsv

paths=$(awk -F '\t' '!/^#/ && $1 != "clause" { print $4 }' R7RS-COMPATIBILITY.tsv | tr ',' '\n')
for path in $paths; do
    [ -e "$path" ] || { echo "missing matrix evidence: $path" >&2; exit 1; }
done
for manifest in tests/r7rs/audit.manifest tests/r7rs/gaps.manifest; do
    files=$(sed '/^[[:space:]]*#/d; /^[[:space:]]*$/d' "$manifest")
    [ -n "$files" ] || { echo "empty manifest: $manifest" >&2; exit 1; }
    for path in $files; do
        [ -f "$path" ] || { echo "missing audit probe: $path" >&2; exit 1; }
        awk -F '\t' -v path="$path" '
          !/^#/ { n=split($4, paths, ","); for (i=1; i<=n; i++) if (paths[i]==path) found=1 }
          END { exit !found }
        ' R7RS-COMPATIBILITY.tsv || { echo "probe absent from matrix: $path" >&2; exit 1; }
    done
done
echo "R7RS matrix evidence and manifests are valid"
