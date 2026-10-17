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

(define-library (liii goldpi)
  (import (scheme base)
    (scheme process-context)
    (scheme write)
    (liii string)
    (liii sys)
    (liii pi)
    (liii goldpi-args)
  ) ;import
  (export main display-pi-help read-all-stdin run-goldpi)
  (begin

    (define (display-pi-help)
      (display "gf pi - AI coding assistant script mode\n\n")
      (display "Usage:\n")
      (display "  gf pi [options] [--] [@files...] [messages...]\n\n")
      (display "Options:\n")
      (display "  -p, --print                    Print response to stdout and exit\n")
      (display "      --mode <mode>              Output mode: text, json (default: text)\n"
      ) ;display
      (display "      --model <id>               Model ID to use\n")
      (display "      --provider <name>          Provider name (e.g. google)\n")
      (display "      --proxy <url>              Proxy URL (e.g. socks5h://127.0.0.1:1080)\n"
      ) ;display
      (display "      --tools <list>             Comma-separated list of tools\n")
      (display "      --max-turns <n>            Maximum turns limit (default: 200, 0 for unlimited)\n"
      ) ;display
      (display "      --timeout <seconds>        Timeout in seconds for bash and HTTP (default: 120)\n"
      ) ;display
      (display "      --no-session               Do not persist session\n")
      (display "      --system-prompt <text>     Override system prompt\n")
      (display "      --append-system-prompt <text>\n")
      (display "                                 Append system prompt section\n")
      (display "  -h, --help                     Show this help message\n")
    ) ;define

    (define (read-all-stdin)
      (let loop
        ((acc '()))
        (let ((chunk (read-string 4096 (current-input-port))))
          (if (eof-object? chunk)
            (let* ((all (apply string-append (reverse acc))) (trimmed (string-trim-both all)))
              (if (string-null? trimmed) #f trimmed)
            ) ;let*
            (loop (cons chunk acc))
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (strip-command-prefix args)
      (if (null? args)
        '()
        (let loop
          ((rest (cdr args)))
          (cond ((null? rest) '())
                ((or (string=? (car rest) "pi") (string-ends? (car rest) "/gf")) (cdr rest))
                (else (loop (cdr rest)))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (run-goldpi raw-args)
      (let* ((argv (strip-command-prefix raw-args)) (parsed (parse-pi-args argv)))
        (if (pi-args-ref parsed 'help)
          (begin
            (display-pi-help)
            0
          ) ;begin
          (let* ((file-text (process-file-arguments (pi-args-ref parsed 'file-args)))
                 (stdin-content (if (not (stdin-tty?)) (read-all-stdin) #f))
                 (messages (pi-args-ref parsed 'messages))
                ) ;
            (let-values (((initial-msg rest-msgs)
                          (build-initial-prompt stdin-content file-text messages)
                         ) ;
                        ) ;
              (if (and (string-null? initial-msg) (null? rest-msgs))
                (begin
                  (display-pi-help)
                  0
                ) ;begin
                (let ((mode (resolve-app-mode (pi-args-ref parsed 'mode)
                              (pi-args-ref parsed 'print)
                              (stdin-tty?)
                              (stdout-tty?)
                            ) ;resolve-app-mode
                      ) ;mode
                     ) ;
                  ;; 阶段一：CLI 骨架建立完毕，后续阶段在此接入 Agent Loop
                  0
                ) ;let
              ) ;if
            ) ;let-values
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (main)
      (run-goldpi (command-line))
    ) ;define

  ) ;begin
) ;define-library
