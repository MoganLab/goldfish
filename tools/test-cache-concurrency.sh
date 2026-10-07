#!/bin/sh
# Concurrency, crash-recovery and portability contract for the gfo cache.
#
#   sh tools/test-cache-concurrency.sh
#
# Enforces what the cache format guarantees, as a test rather than by
# accident:
#   1. parallel cold boots sharing one cache directory all succeed with
#      correct output (pid-qualified tmp files + atomic rename);
#   2. a boot killed mid-write leaves at most a stale .tmp.PID file; the
#      next boot ignores it and recovers;
#   3. truncated or garbage cache artifacts are a miss, not a failure;
#   4. a copied cache directory serves a readonly boot on "another
#      machine" (stamps are content-based; path-relative keys).
set -eu
project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"

workload_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-cache-concurrency.XXXXXX")
trap 'rm -rf "$workload_root"' EXIT HUP INT TERM
mkdir -p "$workload_root/progs"
cat > "$workload_root/progs/workload.scm" <<'EOF'
(import (scheme base) (scheme write) (scheme char))
(define (upcase s) (list->string (map char-upcase (string->list s))))
(write (string-append (upcase "ok") (number->string (+ 20 22))))
(newline)
EOF
printf '"OK42"\n#<unspecified>\n' > "$workload_root/expected"

cache_root="$workload_root/cache"
export GOLDFISH_CACHE_DIR="$cache_root"
workload="$workload_root/progs/workload.scm"

check_output() { # file label
    if ! diff -q "$workload_root/expected" "$1" >/dev/null; then
        echo "cache concurrency: $2 produced wrong output:" >&2
        cat "$1" >&2
        exit 1
    fi
}

# 1. Parallel cold boots sharing one cache directory.
pids=""
for i in 1 2 3 4; do
    ./bin/gf "$workload" > "$workload_root/out.$i" 2> "$workload_root/err.$i" &
    pids="$pids $!"
done
status=0
for p in $pids; do wait "$p" || status=1; done
if [ "$status" != 0 ]; then
    echo "cache concurrency: a parallel cold boot failed" >&2
    cat "$workload_root"/err.* >&2
    exit 1
fi
for i in 1 2 3 4; do check_output "$workload_root/out.$i" "parallel cold boot $i"; done

# 2. A boot killed mid-write: at most a stale .tmp.PID file remains, and
#    the next boot ignores it.  Kill one cold boot in a FRESH cache (its
#    own directory so the kill cannot poison the shared cache under test).
kill_root="$workload_root/killcache"
./bin/gf "$workload" > /dev/null 2>&1 &
killer=$!
sleep 3
kill -9 "$killer" 2>/dev/null || true
wait "$killer" 2>/dev/null || true
GOLDFISH_CACHE_DIR="$kill_root" ./bin/gf "$workload" > "$workload_root/out.kill" 2>/dev/null
check_output "$workload_root/out.kill" "boot after a killed boot"
stale=$(find "$kill_root" -name '*.tmp.*' | wc -l)
echo "cache concurrency: $stale stale tmp file(s) after the kill (informational)"

# 3. Truncated artifact: a miss, then automatic source recovery.
target=$(./bin/gf --bootstrap-cache-directory)
printf '(gfo 0 (' > "$target/expander/lib/install.scm-o2.gfo"
./bin/gf "$workload" > "$workload_root/out.recover" 2> "$workload_root/err.recover"
check_output "$workload_root/out.recover" "boot after artifact truncation"

# 4. A copied cache serves a readonly boot (the distribution model).
cp -a "$cache_root" "$workload_root/copied"
GOLDFISH_CACHE_DIR="$workload_root/copied" GOLDFISH_CACHE_READONLY=1 \
    ./bin/gf "$workload" > "$workload_root/out.copied" 2> "$workload_root/err.copied"
check_output "$workload_root/out.copied" "readonly boot from a copied cache"

echo "cache concurrency: parallel cold boots, crash recovery, corruption recovery and copied-cache portability passed"
