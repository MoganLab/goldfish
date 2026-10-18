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
    (liii goldpi-session)
    (liii goldpi-help)
  ) ;import
  (export main display-pi-help read-all-stdin run-goldpi)
  (begin

    (define (read-all-stdin)
      (let loop
        ((acc '()))
        (let ((chunk (read-string 4096 (current-input-port))))
          (if (or (eof-object? chunk) (= (string-length chunk) 0))
            (let ((trimmed (string-trim-both (apply string-append (reverse acc)))))
              (if (string-null? trimmed) #f trimmed)
            ) ;let
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

    (define (%has-text-block? blks)
      (and (pair? blks)
        (let loop
          ((lst blks))
          (and (not (null? lst))
            (let* ((blk (car lst))
                   (t-val (cdr (or (assoc 'type blk) (assoc "type" blk) '(#f
                                                                          . ""))))
                   (t-text (cdr (or (assoc 'text blk) (assoc "text" blk) '(#f
                                                                           . ""))))
                  ) ;
              (if (and (equal? t-val "text") (not (string-null? t-text))) #t (loop (cdr lst)))
            ) ;let*
          ) ;and
        ) ;let
      ) ;and
    ) ;define

    (define (%print-text-blocks blks)
      (for-each
        (lambda (blk)
          (when
            (equal? (cdr (or (assoc 'type blk) (assoc "type" blk) '(#f . ""))) "text")
            (display (cdr (or (assoc 'text blk) (assoc "text" blk) '(#f . ""))))
            (newline)
          ) ;when
        ) ;lambda
        blks
      ) ;for-each
    ) ;define

    (define (%handle-print-result last-msg)
      (if (not (pair? last-msg))
        (begin
          (%display-stderr "Warning: No assistant response received")
          0
        ) ;begin
        (let ((stop-reason (cdr (or (assoc 'stopReason last-msg) (assoc "stopReason" last-msg) '(#f
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
           ((or (string=? stop-reason "error") (string=? stop-reason "aborted"))
            (%display-stderr (or err-msg (string-append "Request " stop-reason)))
            1
           ) ;
           ((string=? stop-reason "length")
            (%print-text-blocks content)
            (%display-stderr "Response was truncated: the model reached its output token limit."
            ) ;%display-stderr
            1
           ) ;
           ((%has-text-block? content) (%print-text-blocks content) 0)
           (else (%display-stderr "Warning: Model returned no text output") 0)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (%prepare-system-prompt cfg parsed)
      (let* ((cwd (path->string (path-cwd)))
             (sys-override (and (pi-args-ref parsed 'system-prompt)
                             (pi-resolve-prompt-input (pi-args-ref parsed 'system-prompt))
                           ) ;and
             ) ;sys-override
             (appends (map pi-resolve-prompt-input
                        (or (pi-args-ref parsed 'append-system-prompt) '())
                      ) ;map
             ) ;appends
            ) ;
        (pi-build-system-prompt (cdr (assoc 'tools cfg))
          cwd
          (pi-load-project-context cwd)
          sys-override
          appends
        ) ;pi-build-system-prompt
      ) ;let*
    ) ;define

    (define (%persist-initial-user-msg! parsed sdir sfile init-msgs)
      (let ((old-history
              (if (and (pi-args-ref parsed 'continue) sdir)
                (let ((latest (find-latest-session-file sdir)))
                  (if latest (read-session-history latest) '())
                ) ;let
                '()
              ) ;if
            ) ;old-history
           ) ;
        (persist-new-messages! sfile old-history init-msgs)
      ) ;let
    ) ;define

    (define (%run-agent-loop-with-timeout cfg init-msgs system-prompt emit parsed)
      (let ((timeout
              (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                                . 120))))
            ) ;timeout
            (max-turns
              (or (pi-args-ref parsed 'max-turns)
                (cdr (or (assoc 'max-turns cfg) '(#f . 200)))
              ) ;or
            ) ;max-turns
           ) ;
        (run-agent-loop cfg init-msgs system-prompt emit timeout max-turns)
      ) ;let
    ) ;define

    (define (%run-session-flow parsed cfg initial-msg emit on-finish)
      (let*-values (((cwd) (path->string (path-cwd)))
                    ((sfile sdir) (setup-session-file parsed cfg cwd))
                    ((init-msgs) (setup-initial-messages parsed initial-msg sdir))
                   ) ;
        (%persist-initial-user-msg! parsed sdir sfile init-msgs)
        (when emit
          (%emit-json-event 'session cwd)
        ) ;when
        (let ((res-msgs (%run-agent-loop-with-timeout cfg
                          init-msgs
                          (%prepare-system-prompt cfg parsed)
                          emit
                          parsed
                        ) ;%run-agent-loop-with-timeout
              ) ;res-msgs
             ) ;
          (persist-new-messages! sfile init-msgs res-msgs)
          (on-finish res-msgs)
        ) ;let
      ) ;let*-values
    ) ;define

    (define (%emit-json-event ev-type . args)
      (let ((ev (pi-build-json-event ev-type args)))
        (when ev
          (display (pi-serialize-event ev))
          (newline)
        ) ;when
      ) ;let
    ) ;define

    (define (%dispatch-goldpi parsed mode initial-msg rest-msgs)
      (let* ((env (get-environment-variables))
             (cfg (resolve-goldpi-config parsed env))
             (provider (cdr (or (assoc 'provider cfg) '(#f . "google"))))
             (api-key (cdr (or (assoc 'api-key cfg) '(#f . #f))))
            ) ;
        (when (or (not api-key) (and (string? api-key) (string-null? api-key)))
          (error 'credential-error
            (string-append "No credentials found for provider \"" provider "\"")
          ) ;error
        ) ;when
        (case mode
         ((print)
          (%run-session-flow parsed
            cfg
            initial-msg
            #f
            (lambda (res-msgs)
              (%handle-print-result (and (pair? res-msgs) (car (reverse res-msgs))))
            ) ;lambda
          ) ;%run-session-flow
         ) ;
         ((json)
          (%run-session-flow parsed cfg initial-msg %emit-json-event (lambda _ 0))
         ) ;
         (else 0)
        ) ;case
      ) ;let*
    ) ;define

    (define (%run-goldpi-main parsed)
      (let-values (((initial-msg rest-msgs)
                    (build-initial-prompt (and (not (stdin-tty?)) (read-all-stdin))
                      (process-file-arguments (pi-args-ref parsed 'file-args))
                      (pi-args-ref parsed 'messages)
                    ) ;build-initial-prompt
                   ) ;
                  ) ;
        (if
          (or (pi-args-ref parsed 'help)
            (and (string-null? initial-msg)
              (null? rest-msgs)
              (not (pi-args-ref parsed 'continue))
            ) ;and
          ) ;or
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
    ) ;define

    (define (%format-catch-error err-type info)
      (cond
       ((and (pair? info) (string? (car info))) (car info))
       (else (object->string (cons err-type info)))
      ) ;cond
    ) ;define

    (define (run-goldpi raw-args)
      (catch #t
        (lambda () (%run-goldpi-main (parse-pi-args (strip-command-prefix raw-args))))
        (lambda (err-type . info)
          (%display-stderr (string-append "Error: " (%format-catch-error err-type info)))
          1
        ) ;lambda
      ) ;catch
    ) ;define

    (define (main)
      (exit (run-goldpi (command-line)))
    ) ;define

  ) ;begin
) ;define-library
