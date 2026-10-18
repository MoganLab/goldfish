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

(define-library (liii pi modes)
  (import (scheme base)
    (scheme process-context)
    (scheme write)
    (liii string)
    (liii list)
    (liii path)
    (liii pi args)
    (liii pi config)
    (liii pi prompt)
    (liii pi session)
    (liii pi agent)
    (liii pi types)
  ) ;import
  (export display-stderr handle-print-result emit-json-event dispatch-pi
    prepare-system-prompt persist-initial-user-msg! run-agent-loop-with-timeout
    run-session-flow
  ) ;export
  (begin

    (define (display-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define

    (define (%to-block-list blks)
      (cond ((vector? blks) (vector->list blks))
            ((list? blks) blks)
            (else '())
      ) ;cond
    ) ;define

    (define (%has-text-block? blks)
      (let ((lst (%to-block-list blks)))
        (and (pair? lst)
          (any
            (lambda (blk)
              (let ((t (cdr (or (assoc 'type blk) (assoc "type" blk) '(#f . ""))))
                    (text (cdr (or (assoc 'text blk) (assoc "text" blk) '(#f
                                                                          . ""))))
                   ) ;
                (and (equal? t "text") (not (string-null? text)))
              ) ;let
            ) ;lambda
            lst
          ) ;any
        ) ;and
      ) ;let
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
        (%to-block-list blks)
      ) ;for-each
    ) ;define

    (define (handle-print-result last-msg)
      (if (not (or (pair? last-msg) (pi-message? last-msg)))
        (begin
          (display-stderr "Warning: No assistant response received")
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
            (display-stderr (or err-msg (string-append "Request " stop-reason)))
            1
           ) ;
           ((string=? stop-reason "length")
            (%print-text-blocks content)
            (display-stderr "Response was truncated: the model reached its output token limit."
            ) ;display-stderr
            1
           ) ;
           ((%has-text-block? content) (%print-text-blocks content) 0)
           (else (display-stderr "Warning: Model returned no text output") 0)
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    (define (prepare-system-prompt cfg parsed)
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

    (define (persist-initial-user-msg! parsed sdir sfile init-msgs)
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

    (define (run-agent-loop-with-timeout cfg init-msgs system-prompt emit parsed)
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

    (define (emit-json-event ev-type . args)
      (let ((ev (pi-build-json-event ev-type args)))
        (when ev
          (display (pi-serialize-event ev))
          (newline)
        ) ;when
      ) ;let
    ) ;define

    (define (run-session-flow parsed cfg initial-msg emit on-finish)
      (let*-values (((cwd) (path->string (path-cwd)))
                    ((sfile sdir) (setup-session-file parsed cfg cwd))
                    ((init-msgs) (setup-initial-messages parsed initial-msg sdir))
                   ) ;
        (persist-initial-user-msg! parsed sdir sfile init-msgs)
        (when emit
          (emit-json-event 'session cwd)
        ) ;when
        (let ((res-msgs (run-agent-loop-with-timeout cfg
                          init-msgs
                          (prepare-system-prompt cfg parsed)
                          emit
                          parsed
                        ) ;run-agent-loop-with-timeout
              ) ;res-msgs
             ) ;
          (persist-new-messages! sfile init-msgs res-msgs)
          (on-finish res-msgs)
        ) ;let
      ) ;let*-values
    ) ;define

    (define (dispatch-pi parsed mode initial-msg rest-msgs)
      (let* ((env (get-environment-variables))
             (cfg (resolve-pi-config parsed env))
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
          (run-session-flow parsed
            cfg
            initial-msg
            #f
            (lambda (res-msgs)
              (handle-print-result (and (pair? res-msgs) (car (reverse res-msgs))))
            ) ;lambda
          ) ;run-session-flow
         ) ;
         ((json) (run-session-flow parsed cfg initial-msg emit-json-event (lambda _ 0)))
         (else 0)
        ) ;case
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
