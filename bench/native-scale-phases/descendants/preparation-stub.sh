#!/bin/sh
if [ "$1" = tools/warm-bootstrap-cache.sh ]; then
    "$PROBE_REAL_SH" -c 'trap "" TERM; while :; do echo tick >> "$PROBE_HEARTBEAT"; sleep 0.2; done' &
    wait
else
    exec "$PROBE_REAL_SH" "$@"
fi
