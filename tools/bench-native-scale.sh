#!/bin/sh
set -eu
cd "$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)"
profile=quick
samples=3
limit=300
output=
for arg do
    case "$arg" in
      --smoke) profile=quick; samples=1 ;;
      --full) profile=full ;;
      --samples=*) samples=${arg#*=} ;;
      --timeout=*) limit=${arg#*=} ;;
      --output=*) output=${arg#*=} ;;
      *) echo "usage: $0 [--smoke|--full] [--samples=N] [--timeout=N] [--output=DIR]" >&2; exit 2 ;;
    esac
done
for number in "$samples" "$limit"; do
    case "$number" in ''|*[!0-9]*) echo "positive integer required" >&2; exit 2 ;; esac
    [ "$number" -gt 0 ] || { echo "positive integer required" >&2; exit 2; }
done
[ -x bin/gf ]
time_bin=$(which time)
"$time_bin" --version > /dev/null
if [ -z "$output" ]; then output=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-scale.XXXXXX"); fi
mkdir -p "$output"
output=$(CDPATH= cd -- "$output" && pwd)
[ -z "$(ls -A "$output")" ] || { echo "output directory must be empty" >&2; exit 2; }
unset GOLDFISH_DEBUG GOLDFISH_NATIVE_TIMING GOLDFISH_TRACE_THROW
git diff HEAD > "$output/source.patch"
export GOLDFISH_OPT_LEVEL=2
{
    printf 'key\tvalue\n'
    printf 'started_utc\t%s\n' "$(date -u '+%Y-%m-%d %H:%M:%S UTC')"
    printf 'source_commit\t%s\n' "$(git rev-parse HEAD)"
    printf 'binary_sha256\t%s\n' "$(sha256sum bin/gf | cut -d ' ' -f1)"
    printf 'working_diff_sha256\t%s\n' "$(sha256sum "$output/source.patch" | cut -d ' ' -f1)"
    printf 'profile\t%s\nsamples\t%s\ntimeout_seconds\t%s\n' "$profile" "$samples" "$limit"
    printf 'optimization_level\t2\ngc_setting\t%s\n' "${GOLDFISH_GC:-default}"
    printf 'system\t%s\n' "$(uname -srmo)"
    awk -F ': ' '/model name/ {print "cpu\t" $2; exit}' /proc/cpuinfo
    awk '/MemTotal/ {print "memory_kib\t" $2}' /proc/meminfo
    sha256sum bench/native-scale/* tools/bench-native-scale.sh | awk '{print "input_sha256:" $2 "\t" $1}'
} > "$output/metadata.tsv"
printf 'case\tsize\tsample\twall_seconds\tuser_seconds\tsystem_seconds\tpeak_rss_kib\texit_code\tcheck\n' > "$output/results.tsv"
export GOLDFISH_CACHE_DIR="$output/cache"
sh tools/warm-bootstrap-cache.sh > "$output/cache-setup.log" 2>&1
./bin/gf -m liii -I bench -e '(import (native-scale workloads) (native-scale set) (native-scale compile)) #t' \
    > "$output/workload-setup.log" 2>&1
prepared=$(./bin/gf --bootstrap-cache-directory)
for source in workloads set compile; do
    [ -f "$prepared/native-scale/$source.scm-o2.gfo" ] || {
        echo "benchmark workload cache missing: $source" >&2; exit 1;
    }
done
failed=0
tab=$(printf '\t')
while IFS="$tab" read -r name quick full; do
    [ "$name" != case ] || continue
    size=$quick
    [ "$profile" != full ] || size=$full
    export GOLDFISH_BENCH_CASE="$name" GOLDFISH_BENCH_SIZE="$size"
    if [ "$name" = compile-cold ]; then
        export GOLDFISH_BENCH_SOURCE="$output/compile-input.scm"
        perl - "$size" "$GOLDFISH_BENCH_SOURCE" <<'PERL'
use strict; use warnings;
my ($n,$path)=@ARGV; open my $out,'>',$path or die $!;
print {$out} "(import (scheme base))\n";
print {$out} "(define (bench-f-$_ x) (+ x $_))\n" for 0..$n-1;
print {$out} "(unless (and (= (bench-f-0 42) 42) (= (bench-f-",$n-1," 42) ",42+$n-1,")) (error \"compiled benchmark result mismatch\"))\n";
print {$out} "'BENCH-OK\n";
PERL
        sha256sum "$GOLDFISH_BENCH_SOURCE" | awk '{print "input_sha256:compile-input\t" $1}' >> "$output/metadata.tsv"
    fi
    i=1
    while [ "$i" -le "$samples" ]; do
        prefix="$output/$name-$i"
        GOLDFISH_CACHE_DIR="$output/cache"
        case "$name" in
          startup-cold) GOLDFISH_CACHE_DIR="$output/cold-$i"; set -- ./bin/gf -m r7rs -e '(+ 1 1)' ;;
          startup-warm) set -- ./bin/gf -m r7rs -e '(+ 1 1)' ;;
          compile-cold)
            GOLDFISH_CACHE_DIR="$output/compile-cold-$i"
            mkdir -p "$GOLDFISH_CACHE_DIR"
            cp -a "$output/cache"/. "$GOLDFISH_CACHE_DIR"/
            set -- ./bin/gf -m liii -I bench -e '(import (native-scale compile)) (run-compile-workload)' ;;
          compile-warm)
            if [ ! -f "$output/compile-cold-$i.ok" ]; then
                printf '%s\t%s\t%s\tNA\tNA\tNA\tNA\t125\tblocked\n' "$name" "$size" "$i" >> "$output/results.tsv"
                failed=1
                i=$((i + 1))
                continue
            fi
            GOLDFISH_CACHE_DIR="$output/compile-cold-$i"
            set -- ./bin/gf -m liii -I bench -e '(import (native-scale compile)) (run-compile-workload)' ;;
          million-set) set -- ./bin/gf -m r7rs -I bench -e '(import (native-scale set)) (run-set-workload)' ;;
          *) set -- ./bin/gf -m r7rs -I bench -e '(import (native-scale workloads)) (run-scale-workload)' ;;
        esac
        export GOLDFISH_CACHE_DIR
        code=0
        "$time_bin" -f '%e\t%U\t%S\t%M\t%x' -o "$prefix.metrics" \
            timeout --kill-after=5s "${limit}s" "$@" > "$prefix.stdout" 2> "$prefix.stderr" || code=$?
        check=fail
        expected=BENCH-OK
        case "$name" in startup-*) expected=2 ;; esac
        if [ "$code" = 0 ] && [ "$(tail -n 1 "$prefix.stdout")" = "$expected" ]; then
            check=pass
        else failed=1; fi
        case "$name" in compile-*)
            if [ "$check" = pass ]; then
                validation=0
                timeout --kill-after=5s "${limit}s" ./bin/gf -m r7rs "$GOLDFISH_BENCH_SOURCE" \
                    > "$prefix.validation.stdout" 2> "$prefix.validation.stderr" || validation=$?
                printf '%s\n' "$validation" > "$prefix.validation.exit-code"
                if [ "$validation" != 0 ] || [ "$(tail -n 1 "$prefix.validation.stdout")" != BENCH-OK ]; then
                    check=fail
                    failed=1
                fi
            fi ;;
        esac
        if [ "$name" = compile-cold ] && [ "$check" = pass ]; then
            touch "$output/compile-cold-$i.ok"
        fi
        metrics=$(tail -n 1 "$prefix.metrics")
        printf '%s\t%s\t%s\t%s\t%s\n' "$name" "$size" "$i" "$metrics" "$check" >> "$output/results.tsv"
        printf '%s sample %s: %s (exit %s)\n' "$name" "$i" "$check" "$code"
        i=$((i + 1))
    done
done < bench/native-scale/cases.tsv
printf 'ended_utc\t%s\n' "$(date -u '+%Y-%m-%d %H:%M:%S UTC')" >> "$output/metadata.tsv"
printf 'Benchmark evidence: %s\n' "$output"
exit "$failed"
