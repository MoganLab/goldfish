#!/usr/bin/env bash
set -euo pipefail
test "$#" -eq 3
cd "$(dirname "$0")/../.."
root=$1
cache=$2
phase=$3
case "$phase" in
  parse|serialize|lookup|enumerate) ;;
  *) echo "unknown JSON phase: $phase" >&2; exit 2 ;;
esac
mkdir -p "$root"
test -z "$(ls -A "$root")"
mkfifo "$root/control.fifo" "$root/ack.fifo"
trap 'rm -f "$root/control.fifo" "$root/ack.fifo"' EXIT
exec 3<>"$root/control.fifo"
exec 4<>"$root/ack.fifo"
export GOLDFISH_CACHE_DIR="$cache"
export GOLDFISH_DEBUG=timing
perf record -e cpu-clock:u -F 99 --call-graph dwarf,8192 --clockid mono \
  --delay=-1 --control="fifo:$root/control.fifo,$root/ack.fifo" \
  -o "$root/perf.data" -- timeout --foreground --kill-after=5s 180 \
  ./bin/gf -m liii "bench/json-phases/$phase.scm" 2> "$root/run.stderr" |
while IFS= read -r line; do
  printf '%s\n' "$line" >> "$root/run.stdout"
  case "$line" in
    *PHASE-BEGIN$'\t'"$phase")
      printf 'enable\n' >&3
      IFS= read -r -t 5 acknowledgement <&4
      printf 'enable\t%s\n' "$acknowledgement" >> "$root/control.log"
      ;;
    *PHASE$'\t'"$phase"$'\t'*)
      printf 'disable\n' >&3
      IFS= read -r -t 5 acknowledgement <&4
      printf 'disable\t%s\n' "$acknowledgement" >> "$root/control.log"
      ;;
  esac
done
rg -q "^JSON-PHASE-OK $phase$" "$root/run.stdout"
test "$(wc -l < "$root/control.log")" -eq 2
