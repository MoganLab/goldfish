;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes flow)
  (import (scheme base)
    (scheme process-context)
    (scheme write)
    (liii string)
    (liii list)
    (liii path)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code core prompt)
    (liii pi code core session-manager)
    (liii pi agent loop)
    (liii pi ai types)
    (liii pi code core compaction session)
    (liii pi code core compaction flow)
    (liii pi code modes output)
    (liii pi code core extensions types)
    (liii pi code core extensions registry)
  ) ;import
  (export prepare-system-prompt persist-initial-user-msg!
    run-agent-loop-with-timeout emit-json-event run-session-flow
    run-compact-flow
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
        (pi-build-system-prompt (cdr (assoc 'tools cfg))
          cwd
          (pi-load-project-context cwd)
          sys-override
          appends
          custom-info
          session-timeout
          max-turns
        ) ;pi-build-system-prompt
      ) ;let*
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
      (let* ((timeout (pi-resolve-timeout parsed cfg))
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

    (define (%session-context parsed sfile session-file-override)
      ;; 会话文件覆盖（TUI worker 复用同一文件）时始终携带既有上下文；
      ;; 否则仅 --continue 时恢复历史
      (if (and sfile (or session-file-override (pi-args-ref parsed 'continue)))
        (pi-project-session-messages (pi-read-session-entries sfile))
        '()
      ) ;if
    ) ;define

    ;; session-file（可选）：TUI 后台 worker 传入复用的会话文件，
    ;; 同一 TUI 运行内跨消息保持上下文连续（对齐 pi 单会话模型）
    (define (run-session-flow parsed cfg initial-msg emit on-finish . opts)
      (let* ((session-file (and (pair? opts) (car opts))))
        (let*-values (((cwd) (path->string (path-cwd)))
                      ((sfile sdir)
                       (if session-file (values session-file #f) (setup-session-file parsed cfg cwd))
                      ) ;
                      ((context) (%session-context parsed sfile session-file))
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
              (emit 'session cwd)
            ) ;when
            (let* ((state (vector init-msgs (length init-msgs)))
                   (timeout (pi-resolve-timeout parsed cfg))
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
      ) ;let*
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
                 (timeout (pi-resolve-timeout parsed cfg))
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
                  (emit 'session cwd)
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
