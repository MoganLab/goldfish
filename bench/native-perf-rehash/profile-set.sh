#!/usr/bin/env bash
set -euo pipefail
test "$#" -eq 3
cd "$(dirname "$0")/../.."
root=$1
cache=$2
input=$3
mkdir -p "$root"
test -z "$(ls -A "$root")"
cp "$input" "$root/set.input"
mkfifo "$root/control.fifo" "$root/ack.fifo"
trap 'rm -f "$root/control.fifo" "$root/ack.fifo"' EXIT
exec 3<>"$root/control.fifo"
exec 4<>"$root/ack.fifo"
export GOLDFISH_CACHE_DIR="$cache"
export GOLDFISH_DEBUG=timing
perf record -e cpu-clock:u -F 99 --call-graph dwarf,8192 --clockid mono \
  --delay=-1 --control="fifo:$root/control.fifo,$root/ack.fifo" \
  -o "$root/set.data" -- timeout --foreground --kill-after=5s 90 \
  ./bin/gf -m r7rs -I bench < "$root/set.input" 2> "$root/set.stderr" |
while IFS= read -r line; do
  printf '%s\n' "$line" >> "$root/set.stdout"
  case "$line" in
    *PHASE-BEGIN$'\t'profile-set*)
      printf 'enable\n' >&3
      IFS= read -r -t 5 acknowledgement <&4
      printf 'enable\t%s\n' "$acknowledgement" >> "$root/control.log"
      ;;
    *PHASE$'\t'profile-set$'\t'*)
      printf 'disable\n' >&3
      IFS= read -r -t 5 acknowledgement <&4
      printf 'disable\t%s\n' "$acknowledgement" >> "$root/control.log"
      ;;
  esac
done
rg -q '^PROFILE-OK$' "$root/set.stdout"
test "$(wc -l < "$root/control.log")" -eq 2
