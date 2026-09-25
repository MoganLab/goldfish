#!/bin/sh
# Triage a goldtest/gf-native directory-run log: one row per test file
# with its status, plus the first thrown/error line attached to failures.
# Replaces the ad-hoc grep archaeology done per failed run.
#
#   sh tools/triage.sh /tmp/dir.log
set -eu
log=${1:?usage: triage.sh <goldtest-log>}
clean=$(mktemp)
trap 'rm -f "$clean"' EXIT
sed 's/\x1b\[[0-9;]*m//g' "$log" > "$clean"

pass=0
fail=0
grep -E '^  [^ ]+ \.\.\. (PASS|FAIL)$' "$clean" | while IFS= read -r row; do
    file=$(printf '%s\n' "$row" | sed 's/^  //; s/ \.\.\. .*//')
    status=$(printf '%s\n' "$row" | sed 's/.*\.\.\. //')
        if [ "$status" = FAIL ]; then
            why=$(grep -F "while loading $file" "$clean" | head -1 \
                  | sed 's/^.*form [0-9]*: //' | cut -c1-140)
            if [ -z "$why" ]; then
                why=$(grep -F "$file" "$clean" | grep -E 'thrown|error' \
                      | head -1 | cut -c1-140)
            fi
            if [ -z "$why" ]; then
                why=$(grep -E 'correct, [1-9][0-9]* failed' "$clean" \
                      | tail -1 | cut -c1-140)
            fi
            case "$why" in
                *sqrt*|*random*|*inexact*|*1.[0-9]*|*expt*)
                    bucket="float/numeric" ;;
                *stacktrace*|*hook*|*with-let*|*sublet*|*unlet*|*load-expanded*)
                    bucket="s7-compat" ;;
                *"set! of unbound"*|*unbound-variable*)
                    bucket="semantics" ;;
                *) bucket="" ;;
            esac
            printf 'FAIL %s\n' "$file"
            [ -n "$bucket" ] && printf '     [%s]\n' "$bucket"
            printf '     %s\n' "${why:-<no error line captured>}"
    else
        printf 'PASS %s\n' "$file"
    fi
done

echo "---"
echo "PASS: $(grep -cE '^  [^ ]+ \.\.\. PASS$' "$clean")  FAIL: $(grep -cE '^  [^ ]+ \.\.\. FAIL$' "$clean")"
