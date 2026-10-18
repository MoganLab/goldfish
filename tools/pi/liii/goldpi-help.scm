;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(define-library (liii goldpi-help)
  (import (scheme base) (scheme write))
  (export PI_HELP_TEXT display-pi-help)
  (begin
    (define PI_HELP_TEXT
      "gf pi - AI coding assistant script mode\n\nUsage:\n  gf pi [options] [--] [@files...] [messages...]\n\nOptions:\n  -p, --print                    Print response to stdout and exit\n      --mode <mode>              Output mode: text, json (default: text)\n      --model <id>               Model ID to use\n      --provider <name>          Provider name (e.g. google)\n      --proxy <url>              Proxy URL (e.g. socks5h://127.0.0.1:1080)\n      --tools <list>             Comma-separated list of tools\n      --max-turns <n>            Maximum turns limit (default: 200, 0 for unlimited)\n      --timeout <seconds>        Timeout in seconds for bash and HTTP (default: 120)\n  -c, --continue                 Continue previous session\n      --no-session               Do not persist session\n      --system-prompt <text>     Override system prompt\n      --append-system-prompt <text>\n                                 Append system prompt section\n  -h, --help                     Show this help message\n"
    ) ;define

    (define (display-pi-help)
      (display PI_HELP_TEXT)
    ) ;define
  ) ;begin
) ;define-library
