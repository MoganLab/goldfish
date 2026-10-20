;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi modes flow)
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
    (liii pi compact session)
    (liii pi compact flow)
    (liii pi modes output)
    (liii pi code-review)
    (liii pi extension types)
    (liii pi extension registry)
    (liii pi extension builtin prompts)
  ) ;import
  (export prepare-system-prompt persist-initial-user-msg!
    run-agent-loop-with-timeout emit-json-event run-session-flow
    run-compact-flow pi-auto-review-system-prompt
  ) ;export
  (begin

    (define (%collect-custom-tool-info reg)
      (if (not (extension-registry? reg))
        '()
        (map
          (lambda (t)
            (list (extension-tool-def-name t)
              (cons 'snippet (extension-tool-def-snippet t))
              (cons 'guidelines (extension-tool-def-guidelines t))
            ) ;list
          ) ;lambda
          (extension-registry-tools-list reg)
        ) ;map
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
             (reg
               (or (and (pair? cfg) (assoc 'registry cfg) (cdr (assoc 'registry cfg)))
                 (current-extension-registry)
                 (default-extension-registry)
               ) ;or
             ) ;reg
             (custom-info (%collect-custom-tool-info reg))
            ) ;
        (pi-build-system-prompt (cdr (assoc 'tools cfg))
          cwd
          (pi-load-project-context cwd)
          sys-override
          appends
          custom-info
        ) ;pi-build-system-prompt
      ) ;let*
    ) ;define

    (define (pi-auto-review-system-prompt base)
      (lambda (tool-calls)
        (let ((block (pi-build-review-injection (pi-edited-files-from-tool-calls tool-calls)))
             ) ;
          (if block (string-append base block) base)
        ) ;let
      ) ;lambda
    ) ;define

    (define (%system-prompt-for cfg parsed)
      (let* ((base (prepare-system-prompt cfg parsed))
             (reg
               (or (and (pair? cfg) (assoc 'registry cfg) (cdr (assoc 'registry cfg)))
                 (current-extension-registry)
                 (default-extension-registry)
               ) ;or
             ) ;reg
            ) ;
        (lambda (tool-calls)
          (let ((injections (extension-registry-collect-prompt-injectors reg tool-calls)))
            (if (null? injections) base (string-join (cons base injections) "\n\n"))
          ) ;let
        ) ;lambda
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

    (define* (run-agent-loop-with-timeout cfg
               init-msgs
               system-prompt
               emit
               parsed
               (compaction #f)
             ) ;run-agent-loop-with-timeout
      (let* ((timeout
               (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                                 . 120))))
             ) ;timeout
             (max-turns
               (or (pi-args-ref parsed 'max-turns)
                 (cdr (or (assoc 'max-turns cfg) '(#f . 200)))
               ) ;or
             ) ;max-turns
             (session-timeout
               (let ((st (pi-args-ref parsed 'session-timeout)))
                 (if (number? st) st (cdr (or (assoc 'session-timeout cfg) '(#f
                                                                             . 600))))
               ) ;let
             ) ;session-timeout
            ) ;
        (run-agent-loop cfg init-msgs system-prompt emit timeout max-turns
          session-timeout compaction
        ) ;run-agent-loop
      ) ;let*
    ) ;define*

    (define (emit-json-event ev-type . args)
      (let ((ev (pi-build-json-event ev-type args)))
        (when ev
          (display (pi-serialize-event ev))
          (newline)
        ) ;when
      ) ;let
    ) ;define

    (define (%session-context parsed sfile)
      (if (and sfile (pi-args-ref parsed 'continue))
        (pi-project-session-messages (pi-read-session-entries sfile))
        '()
      ) ;if
    ) ;define

    (define (run-session-flow parsed cfg initial-msg emit on-finish)
      (let*-values (((cwd) (path->string (path-cwd)))
                    ((sfile sdir) (setup-session-file parsed cfg cwd))
                    ((context) (%session-context parsed sfile))
                   ) ;
        (let* ((has-prompt? (not (string-null? initial-msg)))
               (init-msgs
                 (append context (if has-prompt? (list (pi-user-message initial-msg)) '()))
               ) ;init-msgs
               (persisted (length context))
              ) ;
          (when has-prompt?
            (persist-new-messages! sfile context init-msgs persisted)
          ) ;when
          (when emit
            (emit-json-event 'session cwd)
          ) ;when
          (let* ((state (vector init-msgs (length init-msgs)))
                 (timeout
                   (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                                     . 120))))
                 ) ;timeout
                 (compaction (pi-make-compaction-option parsed cfg sfile state timeout))
                 (res-msgs (run-agent-loop-with-timeout cfg
                             init-msgs
                             (%system-prompt-for cfg parsed)
                             emit
                             parsed
                             compaction
                           ) ;run-agent-loop-with-timeout
                 ) ;res-msgs
                ) ;
            (persist-new-messages! sfile (vector-ref state 0) res-msgs (vector-ref state 1))
            (on-finish res-msgs)
          ) ;let*
        ) ;let*
      ) ;let*-values
    ) ;define

    (define (run-compact-flow parsed cfg initial-msg emit)
      (let* ((cwd (path->string (path-cwd)))
             (agent-dir (cdr (assoc 'agent-dir cfg)))
             (sdir (get-session-dir agent-dir cwd))
             (sfile (find-latest-session-file sdir))
             (has-prompt? (not (string-null? initial-msg)))
            ) ;
        (if (not sfile)
          (error 'compact-error (string-append "No session found to compact under " sdir))
          (let* ((settings (pi-load-compaction-settings agent-dir))
                 (timeout
                   (or (pi-args-ref parsed 'timeout) (cdr (or (assoc 'timeout cfg) '(#f
                                                                                     . 120))))
                 ) ;timeout
                 (result (pi-compact-session! cfg sfile settings timeout "manual"))
                 (line (pi-format-compaction-summary result "Compacted"))
                ) ;
            (display-stderr line)
            (if (not has-prompt?)
              (begin
                (display line)
                (newline)
                0
              ) ;begin
              (let* ((projection (pi-project-session-messages (pi-read-session-entries sfile)))
                     (init-msgs (append projection (list (pi-user-message initial-msg))))
                     (state (vector init-msgs (length init-msgs)))
                    ) ;
                (persist-new-messages! sfile projection init-msgs (length projection))
                (when emit
                  (emit-json-event 'session cwd)
                ) ;when
                (let* ((compaction (pi-make-compaction-option parsed cfg sfile state timeout))
                       (res-msgs (run-agent-loop-with-timeout cfg
                                   init-msgs
                                   (%system-prompt-for cfg parsed)
                                   emit
                                   parsed
                                   compaction
                                 ) ;run-agent-loop-with-timeout
                       ) ;res-msgs
                      ) ;
                  (persist-new-messages! sfile (vector-ref state 0) res-msgs (vector-ref state 1))
                  (handle-print-result (and (pair? res-msgs) (car (reverse res-msgs))))
                ) ;let*
              ) ;let*
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
