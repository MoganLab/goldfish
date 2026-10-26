;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli help)
  (import (scheme base) (scheme write))
  (export PI_HELP_TEXT display-pi-help)
  (begin
    (define PI_HELP_TEXT
      "gf pi - AI coding assistant script mode\n\nUsage:\n  gf pi [options] [--] [@files...] [messages...]\n\nOptions:\n  -p, --print                    Print response to stdout and exit\n      --mode <mode>              Output mode: text, json (default: text)\n      --model <id>               Model ID to use\n      --provider <name>          Provider name (e.g. google, kimi-coding)\n      --thinking <level>         Thinking level: off, minimal, low, medium, high, xhigh, max\n      --list-models [search]     List available models (with optional search)\n      --proxy <url>              Proxy URL (e.g. socks5h://127.0.0.1:1080)\n      --tools <list>             Comma-separated list of tools\n      --exclude-tools <list>     Comma-separated list of tools to exclude\n      --no-tools                 Disable all tools\n      --no-builtin-tools         Alias for --no-tools\n      --max-turns <n>            Maximum turns limit (default: 200, 0 for unlimited)\n      --timeout <seconds>        Timeout in seconds for bash and HTTP (default: 600)\n      --session-timeout <seconds>\n                                 Total session timeout in seconds (default: 600, 0 to disable)\n  -c, --continue                 Continue previous session\n      --compact                  Compact latest session, then run the prompt (if any)\n      --no-auto-compact          Disable automatic context compaction\n      --no-session               Do not persist session\n      --session-dir <dir>        Directory for session storage and lookup\n                                 (overrides PI_CODING_AGENT_SESSION_DIR\n                                 and sessionDir in settings.json)\n      --code-review              Review uncommitted changes and print a report\n      --simplify                 Review uncommitted changes for simplification (read-only)\n      --apply                    With --simplify, apply within-range changes\n      --staged                   With --code-review/--simplify, review only staged changes\n      --ref <ref>                With --code-review/--simplify, diff against the given ref\n      --auto-review              Inject a per-language review checklist after write/edit\n      --system-prompt <text>     Override system prompt\n      --append-system-prompt <text>\n                                 Append system prompt section\n  -h, --help                     Show this help message\n\nRetry:\n  Retriable provider errors (429/5xx/408/409/network) use the budget from\n  retry.provider.maxRetries in ~/.pi/agent/settings.json,\n  falling back to retry.maxRetries when the nested section is absent.\n"
    ) ;define

    (define (display-pi-help)
      (display PI_HELP_TEXT)
    ) ;define
  ) ;begin
) ;define-library
