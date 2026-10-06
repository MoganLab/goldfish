#!/bin/sh
# Regression gate for the cold-start suite.
#
#   sh bench/cold-start/compare.sh BASELINE_DIR CANDIDATE_DIR [TOLERANCE_PCT]
#
# Compares per-(workload,state) medians of wall time and peak RSS between two
# run-suite output directories.  Exits non-zero if any wall median regresses
# by more than TOLERANCE_PCT (default 10) or any run failed.
set -eu

project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
test "$#" -ge 2 || { echo "usage: sh $0 BASELINE_DIR CANDIDATE_DIR [TOLERANCE_PCT]" >&2; exit 2; }
base=$1
cand=$2
tol=${3:-10}

for d in "$base" "$cand"; do
    test -f "$d/summary.tsv" || { echo "compare: missing $d/summary.tsv" >&2; exit 2; }
done

median_field() { # dir key field
    awk -F'\t' -v k="$2" -v f="$3" 'NR>1 && ($1"|"$2)==k {print $f}' "$1/summary.tsv" \
        | sort -n | awk '{a[NR]=$1} END{if(NR>0) print a[int((NR+1)/2)]}'
}

revision() { awk -F'\t' '$1=="revision"{print $2}' "$1/metadata.tsv" 2>/dev/null || echo "?"; }
binary()   { awk -F'\t' '$1=="binary_sha256"{print substr($2,1,12)}' "$1/metadata.tsv" 2>/dev/null || echo "?"; }

echo "baseline : rev=$(revision "$base") bin=$(binary "$base")"
echo "candidate: rev=$(revision "$cand") bin=$(binary "$cand")"
if [ "$(binary "$base")" = "$(binary "$cand")" ]; then
    echo "warning: same binary hash; this is not an A/B"
fi

keys=$( { tail -n +2 "$base/summary.tsv"; tail -n +2 "$cand/summary.tsv"; } \
        | cut -f1,2 | tr '\t' '|' | sort -u )

printf '%-22s %-10s %10s %10s %8s %8s\n' workload state base_wall cand_wall delta% rss_delta%
fail=0
for k in $keys; do
    bwall=$(median_field "$base" "$k" 4)
    cwall=$(median_field "$cand" "$k" 4)
    wl=${k%|*}; st=${k#*|}
    if [ -z "$bwall" ] || [ -z "$cwall" ]; then
        printf '%-22s %-10s %10s %10s %8s %8s  (no comparison)\n' \
            "$wl" "$st" "${bwall:--}" "${cwall:--}" "-" "-"
        continue
    fi
    brss=$(median_field "$base" "$k" 7)
    crss=$(median_field "$cand" "$k" 7)
    delta=$(awk -v b="$bwall" -v c="$cwall" 'BEGIN{printf "%+.1f", 100*(c-b)/b}')
    rssdelta=$(awk -v b="$brss" -v c="$crss" 'BEGIN{if(b>0) printf "%+.1f", 100*(c-b)/b; else printf "n/a"}')
    flag=""
    if awk -v d="$delta" -v t="$tol" 'BEGIN{exit !(d>t)}'; then flag="  REGRESSION"; fail=1; fi
    printf '%-22s %-10s %10s %10s %8s %8s%s\n' "$wl" "$st" "$bwall" "$cwall" "$delta" "$rssdelta" "$flag"
done

# Any non-zero exit is a correctness failure regardless of timing.
if awk -F'\t' 'NR>1 && $8!=0{exit 0} END{exit 1}' "$cand/summary.tsv"; then
    echo "compare: a candidate run exited non-zero" >&2
    fail=1
fi

[ "$fail" = 0 ] && echo "compare: OK" || echo "compare: FAIL"
exit "$fail"
