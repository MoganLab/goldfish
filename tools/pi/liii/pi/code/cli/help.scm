;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code cli help)
  (import (scheme base) (scheme write) (liii raw-string))
  (export PI_HELP_TEXT display-pi-help)
  (begin
    (define PI_HELP_TEXT
      (string-append (&- #""
          gf pi - AI coding assistant script mode

          Usage:
            gf pi [options] [--] [@files...] [messages...]

          Options:
            -p, --print                    Print response to stdout and exit
                --mode <mode>              Output mode: text, json (default: text)
                --model <id>               Model ID to use
                --provider <name>          Provider to search for --model (requires --model)
                --thinking <level>         Thinking level: off, minimal, low, medium, high, xhigh, max
                --list-models [search]     List available models (with optional search)
                --proxy <url>              Proxy URL (e.g. socks5h://127.0.0.1:1080)
                --tools <list>             Comma-separated list of tools
                --exclude-tools <list>     Comma-separated list of tools to exclude
                --no-tools                 Disable all tools
                --no-builtin-tools         Alias for --no-tools
                --max-turns <n>            Maximum turns limit (0 or unset for unlimited)
                --timeout <seconds>        Timeout in seconds for bash and HTTP (default: 600)
                --session-timeout <seconds>
                                           Total session timeout in seconds (default: 600, 0 to disable)
            -c, --continue                 Continue previous session
            -r, --resume                   Select a session to resume
            -n, --name <name>              Set session display name
                --compact                  Compact latest session, then run the prompt (if any)
                --no-auto-compact          Disable automatic context compaction
                --no-session               Do not persist session
                --session-dir <dir>        Directory for session storage and lookup
                                           (overrides PI_CODING_AGENT_SESSION_DIR
                                           and sessionDir in settings.json)
                --code-review              Review uncommitted changes and print a report
                --simplify                 Review uncommitted changes for simplification (read-only)
                --apply                    With --simplify, apply within-range changes
                --staged                   With --code-review/--simplify, review only staged changes
                --ref <ref>                With --code-review/--simplify, diff against the given ref
                --auto-review              Inject a per-language review checklist after write/edit
                --system-prompt <text>     Override system prompt
                --append-system-prompt <text>
                                           Append system prompt section
            -h, --help                     Show this help message

          Retry:
            Retriable provider errors (429/5xx/408/409/network) use the budget from
            retry.provider.maxRetries in ~/.pi/agent/settings.json,
            falling back to retry.maxRetries when the nested section is absent.
          ""
                     ) ;&-
        "\n"
      ) ;string-append
    ) ;define

    (define (display-pi-help)
      (display PI_HELP_TEXT)
    ) ;define
  ) ;begin
) ;define-library
