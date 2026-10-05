#!/bin/sh
# Exercise source-install cache replay and recovery in an isolated cache.
set -eu
project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"
seed=$(bin/gf --check-bootstrap-cache)
test_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-startup-cache.XXXXXX")
trap 'rm -rf "$test_root"' EXIT HUP INT TERM
export GOLDFISH_CACHE_DIR="$test_root/cache"
export GOLDFISH_OPT_LEVEL=2
target=$(bin/gf --bootstrap-cache-directory)
mkdir -p "$target"
cp -a "$seed"/. "$target"/
reader=$target/liii/reader.scm-o2.gfo
writer=$target/expander/lib/native-write.scm-o2.gfo
rm -f "$reader" "$writer"

probe='(import (scheme base) (scheme read) (scheme write))
       (let* ((p (open-input-string "#!fold-case MiXeD |MiXeD| 3/2 \"中🙂\""))
               (v (vector #f)) (out (open-output-string)))
          (unless (and (eq? (read p) (quote mixed))
                       (eq? (read p) (quote MiXeD))
                       (= (read p) 3/2) (equal? (read p) "中🙂"))
            (error "startup reader regression"))
          (vector-set! v 0 v)
          (write-shared v out)
          (let ((copy (read (open-input-string (get-output-string out)))))
            (unless (eq? copy (vector-ref copy 0))
              (error "startup writer regression")))
          42)'
run_probe() {
    result=$(timeout -k 5 90 bin/gf -m "$1" -e "$probe")
    test "$result" = 42
}

# Missing entries expand from source, then all modes replay the same entries.
run_probe r7rs
test -s "$reader"
test -s "$writer"
reader_time=$(stat -c %Y "$reader")
writer_time=$(stat -c %Y "$writer")
for mode in r7rs liii scheme sicp s7; do run_probe "$mode"; done
test "$(stat -c %Y "$reader")" = "$reader_time"
test "$(stat -c %Y "$writer")" = "$writer_time"

# A syntactically corrupt entry and a stale source digest are cache misses.
printf '(gfo 0 (' > "$reader"
run_probe r7rs
digest=$(md5sum goldfish/expander/lib/native-write.scm | cut -d ' ' -f 1)
sed "s/$digest/00000000000000000000000000000000/" "$writer" > "$test_root/stale"
if cmp -s "$writer" "$test_root/stale"; then
    echo "startup cache: source digest was not present" >&2
    exit 1
fi
mv "$test_root/stale" "$writer"
run_probe r7rs
test "$(stat -c %Y "$writer")" != "$writer_time"

# Unwritable caches retain the native source fallback.
rm "$reader" "$writer"
export GOLDFISH_CACHE_READONLY=1
run_probe r7rs
test ! -e "$reader"
test ! -e "$writer"
echo "native startup cache: replay, modes, corruption, stamps and read-only fallback passed"
