#!/bin/sh
# C2 parity runner: run a batch of files through BOTH harnesses in one
# invocation each (goldtest accepts many files; native forks a booted
# process per file), then compare per-file verdict rows -- the only
# signal both harnesses print.
#
#   tools/c2-compare.sh                      # whole manifest
#   tools/c2-compare.sh f1.scm f2.scm        # an explicit slice
#   C2_SKIP_MEGA=1 ...                       # skip known-slow audits
#   C2_MEM=10485760 ...                      # native ulimit -v (KB)
#   C2_BATCH=40 ...                          # files per invocation
set -eu
project=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project"

mem=${C2_MEM:-8388608}
batch=${C2_BATCH:-40}
mega_re='export-strict-audit-test|lib-cache-all-libs-test'

bucketed=0
skip_file=${C2_SKIP_MANIFEST:-tests/c2-skip.tsv}
skip_paths=""
if [ -f "$skip_file" ]; then
    skip_paths=$(grep -v '^#' "$skip_file" | awk '{print $1}')
    bucketed=$(grep -vc '^#' "$skip_file")
fi
if [ $# -gt 0 ]; then
    raw=$(printf '%s\n' "$@")
else
    raw=$(cat "${C2_MANIFEST:-tests/float-free.manifest}")
fi
# Skip entries are global: bucketed files never run, even explicitly.
files=$(printf '%s\n' "$raw" | while IFS= read -r f; do
    if [ -n "$skip_paths" ] && printf '%s\n' "$skip_paths" | grep -qxF "$f"; then
        continue
    fi
    if [ "${C2_SKIP_MEGA:-0}" = 1 ] && printf '%s' "$f" | grep -qE "$mega_re"; then
        continue
    fi
    echo "$f"
done)
total=$(printf '%s\n' "$files" | grep -c . || true)

classify() {
    case "$1" in
        *sqrt*|*random*|*inexact*|*1.[0-9]*|*expt*) echo "float/numeric" ;;
        *stacktrace*|*hook*|*with-let*|*sublet*|*unlet*|*load-expanded*) echo "s7-compat" ;;
        *"set! of unbound"*|*unbound-variable*) echo "semantics" ;;
        *call/cc*|*call-with-current-continuation*) echo "engine-callcc" ;;
        *) echo "" ;;
    esac
}

# verdicts <side> <log> : one "file|VERDICT" line per verdict row
verdicts() {
    sed 's/\x1b\[[0-9;]*m//g' "$2" | grep -E '^  [^ ]+ \.\.\. (PASS|FAIL)$' \
        | while IFS= read -r row; do
            f=$(printf '%s\n' "$row" | sed 's/^  //; s/ \.\.\. .*//')
            v=$(printf '%s\n' "$row" | sed 's/.*\.\.\. //')
            printf '%s|%s\n' "$f" "$v"
        done
}

run_side() { # $1=host|native  $2=log  $3...=files
    side=$1; log=$2; shift 2
    : > "$log"
    if [ "$side" = host ]; then
        # goldtest parses one path per invocation (by design); host
        # invocations are cheap and stay process-isolated.
        for f in "$@"; do
            timeout "${C2_HOST_TIMEOUT:-300}" ./bin/gf test "$f" \
                >> "$log" 2>&1 || true
        done
    else
        # One boot, one fork per file: no goldtest tool reload.
        ( ulimit -v "$mem"
          timeout "${C2_TIMEOUT:-3600}" ./bin/gf-native -m liii \
            --each-file "$@" ) >> "$log" 2>&1 || true
    fi
    verdicts "$side" "$log"
}

# process one batch: $1... = files
agree_pass=0; agree_fail=0; diverge=0; missing=0
: > /tmp/c2-diverge.log
batch_files=""
process_batch() {
    [ -z "$batch_files" ] && return 0
    bfiles=$(printf '%s\n' "$batch_files")
    # shellcheck disable=SC2086
    h_log=$(mktemp); n_log=$(mktemp)
    h_v=$(run_side host "$h_log" $bfiles)
    n_v=$(run_side native "$n_log" $bfiles)
    for f in $bfiles; do
        hv=$(printf '%s\n' "$h_v" | awk -F'|' -v f="$f" '$1==f {print $2; exit}')
        nv=$(printf '%s\n' "$n_v" | awk -F'|' -v f="$f" '$1==f {print $2; exit}')
        if [ -z "$hv" ] || [ -z "$nv" ]; then
            missing=$((missing + 1))
            printf 'MISSING    %s  host[%s] native[%s]\n' "$f" "${hv:-no-row}" "${nv:-no-row}"
            continue
        fi
        if [ "$hv" = "$nv" ]; then
            if [ "$hv" = PASS ]; then
                agree_pass=$((agree_pass + 1))
            else
                agree_fail=$((agree_fail + 1))
                printf 'AGREE-FAIL  %s\n' "$f"
            fi
        else
            diverge=$((diverge + 1))
            why=$(grep -F "$f" "$n_log" | grep -E 'while loading|thrown:|failed' | head -1 | cut -c1-140)
            bucket=$(classify "$why")
            printf 'DIVERGE     %s  host[%s] native[%s]\n' "$f" "$hv" "$nv"
            [ -n "$bucket" ] && printf '            bucket=%s  %s\n' "$bucket" "$why"
            printf '%s\thost=%s\tnative=%s\t%s\n' "$f" "$hv" "$nv" "$why" >> /tmp/c2-diverge.log
        fi
    done
    rm -f "$h_log" "$n_log"
    batch_files=""
}

for f in $files; do
    batch_files="${batch_files}${batch_files:+
}${f}"
    n=$(printf '%s\n' "$batch_files" | grep -c . || true)
    if [ "$n" -ge "$batch" ]; then
        process_batch
    fi
done
process_batch

echo "---"
echo "total=$total agree-pass=$agree_pass agree-fail=$agree_fail diverge=$diverge missing=$missing manifest-bucketed=$bucketed"
echo "divergence rows: /tmp/c2-diverge.log"
