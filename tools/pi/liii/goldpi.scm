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
    (liii list)
    (liii time)
    (liii path)
    (liii sys)
    (liii pi)
    (liii goldpi-args)
    (liii goldpi-config)
    (liii goldpi-agent)
  ) ;import
  (export main display-pi-help read-all-stdin run-goldpi)
  (begin

    (define (display-pi-help)
      (display (string-append "gf pi - AI coding assistant script mode\n\n"
                 "Usage:\n  gf pi [options] [--] [@files...] [messages...]\n\n"
                 "Options:\n"
                 "  -p, --print                    Print response to stdout and exit\n"
                 "      --mode <mode>              Output mode: text, json (default: text)\n"
                 "      --model <id>               Model ID to use\n"
                 "      --provider <name>          Provider name (e.g. google)\n"
                 "      --proxy <url>              Proxy URL (e.g. socks5h://127.0.0.1:1080)\n"
                 "      --tools <list>             Comma-separated list of tools\n"
                 "      --max-turns <n>            Maximum turns limit (default: 200, 0 for unlimited)\n"
                 "      --timeout <seconds>        Timeout in seconds for bash and HTTP (default: 120)\n"
                 "      --no-session               Do not persist session\n"
                 "      --system-prompt <text>     Override system prompt\n"
                 "      --append-system-prompt <text>\n"
                 "                                 Append system prompt section\n"
                 "  -h, --help                     Show this help message\n"
               ) ;string-append
      ) ;display
    ) ;define

    (define (read-all-stdin)
      (let loop
        ((acc '()))
        (let ((chunk (read-string 4096 (current-input-port))))
          (if (or (eof-object? chunk) (= (string-length chunk) 0))
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

    (define (%display-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define

    (define (%print-text-blocks blks)
      (for-each
        (lambda (blk)
          (let ((t-val (cdr (or (assoc 'type blk) (assoc "type" blk) '(#f . "")))))
            (when (equal? t-val "text")
              (let ((t (cdr (or (assoc 'text blk) (assoc "text" blk) '(#f . "")))))
                (display t)
                (newline)
              ) ;let
            ) ;when
          ) ;let
        ) ;lambda
        blks
      ) ;for-each
    ) ;define

    (define (%handle-print-result last-msg)
      (if (not (pair? last-msg))
        0
        (let* ((stop-reason (cdr (or (assoc 'stopReason last-msg) (assoc "stopReason" last-msg) '(#f
                                                                                                  . "stop"))
                            ) ;cdr
               ) ;stop-reason
               (content (cdr (or (assoc 'content last-msg) (assoc "content" last-msg) '(#f))))
               (err-msg (cdr (or (assoc 'errorMessage last-msg) (assoc "errorMessage" last-msg) '(#f
                                                                                                  . #f))
                        ) ;cdr
               ) ;err-msg
              ) ;
          (cond
           ((or (equal? stop-reason "error") (equal? stop-reason "aborted"))
            (%display-stderr (or err-msg (string-append "Request " stop-reason)))
            1
           ) ;
           ((equal? stop-reason "length")
            (%print-text-blocks content)
            (%display-stderr "Response was truncated: the model reached its output token limit."
            ) ;%display-stderr
            1
           ) ;
           (else (%print-text-blocks content) 0)
          ) ;cond
        ) ;let*
      ) ;if
    ) ;define

    (define (%prepare-system-prompt cfg parsed)
      (let* ((cwd (path->string (path-cwd)))
             (project-ctx (pi-load-project-context cwd))
             (raw-override (pi-args-ref parsed 'system-prompt))
             (sys-override (if raw-override (pi-resolve-prompt-input raw-override) #f))
             (raw-appends (or (pi-args-ref parsed 'append-system-prompt) '()))
             (append-prompts (map pi-resolve-prompt-input raw-appends))
             (tools (cdr (assoc 'tools cfg)))
            ) ;
        (pi-build-system-prompt tools cwd project-ctx sys-override append-prompts)
      ) ;let*
    ) ;define

    (define (%run-agent-print-session cfg parsed initial-msg)
      (let* ((system-prompt (%prepare-system-prompt cfg parsed))
             (init-msgs
               (list (list (cons 'role "user")
                       (cons 'content initial-msg)
                       (cons 'timestamp (pi-current-time-millis))
                     ) ;list
               ) ;list
             ) ;init-msgs
             (timeout
               (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                                 . 120))))
             ) ;timeout
             (max-turns
               (or (pi-args-ref parsed 'max-turns)
                 (cdr (or (assoc 'max-turns cfg) '(#f . 200)))
               ) ;or
             ) ;max-turns
             (res-msgs (run-agent-loop cfg init-msgs system-prompt #f timeout max-turns))
             (last-msg (if (pair? res-msgs) (car (reverse res-msgs)) #f))
            ) ;
        (%handle-print-result last-msg)
      ) ;let*
    ) ;define

    (define (%build-json-event ev-type args)
      (case ev-type
       ((session) (apply pi-event-session args))
       ((agent_start) (pi-event-agent-start))
       ((turn_start) (pi-event-turn-start))
       ((message_start) (pi-event-message-start (car args)))
       ((message_update) (pi-event-message-update (car args) (cadr args)))
       ((message_end) (pi-event-message-end (car args)))
       ((tool_execution_start) (apply pi-event-tool-execution-start args))
       ((tool_execution_update) (apply pi-event-tool-execution-update args))
       ((tool_execution_end) (apply pi-event-tool-execution-end args))
       ((turn_end) (pi-event-turn-end (car args) (cadr args)))
       ((agent_end) (pi-event-agent-end (car args) (cadr args)))
       ((agent_settled) (pi-event-agent-settled (car args)))
       (else #f)
      ) ;case
    ) ;define

    (define (%emit-json-event ev-type . args)
      (let ((ev (%build-json-event ev-type args)))
        (when ev
          (display (pi-serialize-event ev))
          (newline)
        ) ;when
      ) ;let
    ) ;define

    (define (%run-agent-json-session cfg parsed initial-msg)
      (let* ((cwd (path->string (path-cwd)))
             (system-prompt (%prepare-system-prompt cfg parsed))
             (init-msgs
               (list (list (cons 'role "user")
                       (cons 'content initial-msg)
                       (cons 'timestamp (pi-current-time-millis))
                     ) ;list
               ) ;list
             ) ;init-msgs
             (timeout
               (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                                 . 120))))
             ) ;timeout
             (max-turns
               (or (pi-args-ref parsed 'max-turns)
                 (cdr (or (assoc 'max-turns cfg) '(#f . 200)))
               ) ;or
             ) ;max-turns
            ) ;
        (%emit-json-event 'session cwd)
        (run-agent-loop cfg init-msgs system-prompt %emit-json-event timeout max-turns)
        0
      ) ;let*
    ) ;define

    (define (%dispatch-goldpi parsed mode initial-msg rest-msgs)
      (let* ((env (get-environment-variables)) (cfg (resolve-goldpi-config parsed env)))
        (cond ((eq? mode 'print) (%run-agent-print-session cfg parsed initial-msg))
              ((eq? mode 'json) (%run-agent-json-session cfg parsed initial-msg))
              (else 0)
        ) ;cond
      ) ;let*
    ) ;define

    (define (run-goldpi raw-args)
      (catch #t
        (lambda ()
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
                      (%dispatch-goldpi parsed mode initial-msg rest-msgs)
                    ) ;let
                  ) ;if
                ) ;let-values
              ) ;let*
            ) ;if
          ) ;let*
        ) ;lambda
        (lambda (err-type . info)
          (let ((msg
                  (if (and (pair? info) (string? (car info)))
                    (car info)
                    (object->string (cons err-type info))
                  ) ;if
                ) ;msg
               ) ;
            (%display-stderr (string-append "Error: " msg))
            1
          ) ;let
        ) ;lambda
      ) ;catch
    ) ;define

    (define (main)
      (exit (run-goldpi (command-line)))
    ) ;define

  ) ;begin
) ;define-library
