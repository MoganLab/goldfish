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
#   C2_HOST_WORKER=1 ...                     # reuse goldtest worker for safe files
#   C2_NATIVE_JOBS=2 ...                     # parallel native process groups
#   C2_STRICT=1 ...                          # fail on divergence, missing verdict, or shared failure
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
    raw=$(awk '!/^[[:space:]]*#/ && NF { print $1 }' \
        "${C2_MANIFEST:-tests/float-free.manifest}")
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
        if [ "${C2_HOST_WORKER:-0}" = 1 ]; then
            # The persistent goldtest worker resets the program library and
            # check state between files. Use only for slices whose files are
            # worker-safe; default remains one isolated gf test per file.
            worker_verdicts=$(mktemp)
            GOLDFISH_CHECK_NO_EXIT=1 \
                timeout "${C2_HOST_WORKER_TIMEOUT:-3600}" \
                ./bin/gf-host -m liii tools/test/liii/worker.scm -- "$@" \
                >> "$log" 2>&1 || true
            sed 's/\x1b\[[0-9;]*m//g' "$log" \
                | awk '/^;;;WORKER / { print "  " $2 " ... " ($3 == 0 ? "PASS" : "FAIL") }' \
                > "$worker_verdicts"
            cat "$worker_verdicts" >> "$log"
            rm -f "$worker_verdicts"
        else
            # The current host CLI has no `test` subcommand. Load one test in
            # an isolated host process; check-report is the per-file verdict
            # emitted by the Scheme test harness.
            for f in "$@"; do
                host_log=$(mktemp)
                if timeout "${C2_HOST_TIMEOUT:-300}" ./bin/gf-host -m liii \
                    -e "(load \"$f\")" > "$host_log" 2>&1; then
                    cat "$host_log" >> "$log"
                    if grep -Eq '\*\*\* checks \*\*\* : [0-9]+ correct, 0 failed\.' \
                        "$host_log"; then
                        printf '  %s ... PASS\n' "$f" >> "$log"
                    else
                        printf '  %s ... FAIL\n' "$f" >> "$log"
                    fi
                else
                    cat "$host_log" >> "$log"
                    printf '  %s ... FAIL\n' "$f" >> "$log"
                fi
                rm -f "$host_log"
            done
        fi
    else
        native_jobs=${C2_NATIVE_JOBS:-1}
        case "$native_jobs" in
            ''|*[!0-9]*|0) echo "C2_NATIVE_JOBS must be a positive integer" >&2; return 2 ;;
        esac
        if [ "$native_jobs" -eq 1 ]; then
            # One boot, one fork per file: no goldtest tool reload.
            ( ulimit -v "$mem"
              timeout "${C2_TIMEOUT:-3600}" ./bin/gf -m liii \
                --each-file "$@" ) >> "$log" 2>&1 || true
        else
            # Separate booted processes keep each file isolated while
            # allowing an explicit, memory-budgeted parallel sweep.
            worker_dir=$(mktemp -d)
            index=0
            for f in "$@"; do
                job=$((index % native_jobs))
                printf '%s\n' "$f" >> "$worker_dir/files.$job"
                index=$((index + 1))
            done
            pids=""
            job=0
            while [ "$job" -lt "$native_jobs" ]; do
                if [ -s "$worker_dir/files.$job" ]; then
                    (
                        set -- $(cat "$worker_dir/files.$job")
                        ulimit -v "$mem"
                        timeout "${C2_TIMEOUT:-3600}" \
                            ./bin/gf -m liii --each-file "$@"
                    ) > "$worker_dir/out.$job" 2>&1 &
                    pids="$pids $!"
                fi
                job=$((job + 1))
            done
            for pid in $pids; do wait "$pid" || true; done
            job=0
            while [ "$job" -lt "$native_jobs" ]; do
                if [ -f "$worker_dir/out.$job" ]; then
                    cat "$worker_dir/out.$job" >> "$log"
                fi
                job=$((job + 1))
            done
            rm -rf "$worker_dir"
        fi
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

if [ "${C2_STRICT:-0}" = 1 ] && { [ "$agree_fail" -ne 0 ] || [ "$diverge" -ne 0 ] || [ "$missing" -ne 0 ]; }; then
    echo "C2 strict gate failed" >&2
    exit 1
fi
