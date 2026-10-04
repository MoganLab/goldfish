#!/bin/sh
set -eu
project_dir=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_dir"
fixture_root=$(mktemp -d "${TMPDIR:-/tmp}/goldfish-declaration-cache.XXXXXX")
trap 'rm -rf "$fixture_root"' EXIT HUP INT TERM
mkdir -p "$fixture_root/declaration-cache/parts"
export GOLDFISH_OPT_LEVEL=2
cat > "$fixture_root/declaration-cache/provider.scm" <<'SCM'
(define-library (declaration-cache provider)
  (include-library-declarations "parts/declarations.scm"))
SCM
cat > "$fixture_root/declaration-cache/parts/declarations.scm" <<'SCM'
(import (scheme base) (only (declaration-cache 7) base-answer))
(export answer-macro)
(include "macro.scm")
SCM
cat > "$fixture_root/declaration-cache/7.scm" <<'SCM'
(define-library (declaration-cache 7)
  (import (scheme base)) (export base-answer)
  (begin (define base-answer 39)))
SCM
cat > "$fixture_root/declaration-cache/parts/macro.scm" <<'SCM'
(define-syntax answer-macro (syntax-rules () ((_ ) (+ base-answer 2))))
SCM
cat > "$fixture_root/consumer.scm" <<'SCM'
(import (scheme base) (declaration-cache provider))
(answer-macro)
SCM
run() { ./bin/gf -m r7rs -I "$fixture_root" "$fixture_root/$1"; }
expect() {
    actual=$(run "$1")
    if [ "$actual" != "$2" ]; then
        echo "declaration cache: $1 returned $actual, expected $2" >&2
        exit 1
    fi
}
expect consumer.scm 41
expect consumer.scm 41
sed 's/base-answer 2/base-answer 3/' "$fixture_root/declaration-cache/parts/macro.scm" > "$fixture_root/new.scm"
mv "$fixture_root/new.scm" "$fixture_root/declaration-cache/parts/macro.scm"
expect consumer.scm 42
mv "$fixture_root/declaration-cache/parts/macro.scm" "$fixture_root/saved.scm"
if run consumer.scm >/dev/null 2>&1; then
    echo "declaration cache: reused a consumer with a missing include" >&2; exit 1
fi
mv "$fixture_root/saved.scm" "$fixture_root/declaration-cache/parts/macro.scm"
expect consumer.scm 42

cat > "$fixture_root/availability.scm" <<'SCM'
(import (scheme base))
(cond-expand ((library (declaration-cache optional)) 1) (else 0))
SCM
expect availability.scm 0
cat > "$fixture_root/declaration-cache/optional.scm" <<'SCM'
(define-library (declaration-cache optional)
  (import (scheme base))
  (begin (error "availability queries must not execute library bodies")))
SCM
expect availability.scm 1
rm "$fixture_root/declaration-cache/optional.scm"
expect availability.scm 0

cat > "$fixture_root/expression.scm" <<'SCM'
(import (scheme base))
(include-ci "folded.scm")
expr-value
SCM
printf '(DEFINE EXPR-VALUE 42)\n' > "$fixture_root/folded.scm"
expect expression.scm 42
printf '(DEFINE EXPR-VALUE 43)\n' > "$fixture_root/folded.scm"
expect expression.scm 43

echo "library declaration cache: warm replay, transitive includes and availability changes passed"
