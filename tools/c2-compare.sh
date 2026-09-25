#!/bin/sh
# C2 parity runner: run every given file (or the manifest) through both
# runtimes and compare exit status plus the check summary.  Rows are
# machine-readable; DIVERGE rows carry a first-guess bucket.
#
#   tools/c2-compare.sh                      # whole manifest
#   tools/c2-compare.sh tests/expander/*.scm # a slice
#   C2_SKIP_MEGA=1 tools/c2-compare.sh ...   # skip known-slow audits
#   C2_MEM=10485760 ...                      # native ulimit -v (KB)
#
# Host results cache: put the host side in a file once (it is the fast,
# stable side) with C2_HOST_LOG=baseline.tsv to avoid re-paying it.
set -eu
project=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project"

mem=${C2_MEM:-8388608}
mega_re='export-strict-audit-test|lib-cache-all-libs-test'

bucketed=0
skip_file=${C2_SKIP_MANIFEST:-tests/c2-skip.tsv}
skip_paths=""
if [ -f "$skip_file" ]; then
    skip_paths=$(grep -v '^#' "$skip_file" | awk '{print $1}')
    bucketed=$(grep -vc '^#' "$skip_file")
fi
if [ $# -gt 0 ]; then
    raw="$*"
else
    raw=$(cat "${C2_MANIFEST:-tests/float-free.manifest}")
fi
# Skip entries are global: bucketed files never run, even when passed
# explicitly.
files=$(printf '%s\n' "$raw" | while IFS= read -r f; do
    if [ -n "$skip_paths" ] && printf '%s\n' "$skip_paths" | grep -qxF "$f"; then
        continue
    fi
    echo "$f"
done)

classify() { # $1 = both-fail marker is handled by caller
    case "$1" in
        *sqrt*|*random*|*inexact*|*1.[0-9]*|*expt*) echo "float/numeric" ;;
        *stacktrace*|*hook*|*with-let*|*sublet*|*unlet*|*load-expanded*) echo "s7-compat" ;;
        *"set! of unbound"*|*unbound-variable*) echo "semantics" ;;
        *call/cc*|*call-with-current-continuation*) echo "engine-callcc" ;;
        *) echo "" ;;
    esac
}

side() { # $1=host|native $2=file -> "rc|verdict|why"
    out=""
    rc=0
    if [ "$1" = host ]; then
        out=$(timeout "${C2_HOST_TIMEOUT:-300}" ./bin/gf test "$2" 2>&1) || rc=$?
    else
        out=$( (ulimit -v "$mem"; timeout "${C2_TIMEOUT:-420}" ./bin/gf-native test "$2" 2>&1) ) || rc=$?
    fi
    # Common denominator across the two harnesses: host prints only the
    # verdict row, native streams the checks too.  Fall back to rc when
    # a side died before its summary.
    verdict=$(printf '%s\n' "$out" | sed 's/\x1b\[[0-9;]*m//g' \
              | grep -F "$2 ... " | tail -1 | sed 's/.*\.\.\. //')
    if [ -z "$verdict" ]; then
        if [ "$rc" = 0 ]; then verdict=PASS; else verdict=FAIL; fi
        verdict="$verdict(no-row)"
    fi
    why=$(printf '%s\n' "$out" | sed 's/\x1b\[[0-9;]*m//g' \
          | grep -E 'while loading|thrown:|failed' | head -1 | cut -c1-140)
    printf '%s|%s|%s\n' "$rc" "$verdict" "$why"
}

agree_pass=0
agree_fail=0
diverge=0
skipped=0
: > /tmp/c2-diverge.log
for f in $files; do
    if [ "${C2_SKIP_MEGA:-0}" = 1 ] && printf '%s' "$f" | grep -qE "$mega_re"; then
        skipped=$((skipped + 1))
        continue
    fi
    h=$(side host "$f")
    n=$(side native "$f")
    h_rc=${h%%|*}; h_rest=${h#*|}; h_verdict=${h_rest%%|*}; h_why=${h_rest#*|}
    n_rc=${n%%|*}; n_rest=${n#*|}; n_verdict=${n_rest%%|*}; n_why=${n_rest#*|}
    h_status=${h_verdict%%(*}; n_status=${n_verdict%%(*}
    if [ "$h_status" = "$n_status" ]; then
        if [ "$h_status" = PASS ]; then
            agree_pass=$((agree_pass + 1))
            printf 'AGREE-PASS  %s\n' "$f"
        else
            agree_fail=$((agree_fail + 1))
            printf 'AGREE-FAIL  %s  host:%s native:%s\n' "$f" "$h_verdict" "$n_verdict"
        fi
    else
        diverge=$((diverge + 1))
        bucket=$(classify "$n_why$h_why")
        printf 'DIVERGE     %s  host[rc=%s %s] native[rc=%s %s]\n' \
            "$f" "$h_rc" "$h_verdict" "$n_rc" "$n_verdict"
        [ -n "$bucket" ] && printf '            bucket=%s  %s\n' "$bucket" "$n_why$h_why"
        printf '%s\thost=%s/%s\tnative=%s/%s\t%s\n' \
            "$f" "$h_rc" "$h_verdict" "$n_rc" "$n_verdict" "$n_why" >> /tmp/c2-diverge.log
    fi
done
echo "---"
echo "agree-pass=$agree_pass agree-fail=$agree_fail diverge=$diverge skipped=$skipped manifest-bucketed=$bucketed"
echo "divergence rows: /tmp/c2-diverge.log"
