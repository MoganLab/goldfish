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
    (liii pi code core session-name)
    (liii pi agent loop)
    (liii pi ai dispatch)
    (liii pi ai types)
    (liii pi code core compaction session)
    (liii pi code core compaction flow)
    (liii pi code core session-projection)
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
                 (cdr (or (assoc 'max-turns cfg) (cons #f 0)))
               ) ;or
             ) ;max-turns
             (session-timeout
               (let ((st (pi-args-ref parsed 'session-timeout)))
                 (if (number? st) st (cdr (or (assoc 'session-timeout cfg) '(#f
                                                                             . 0))))
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
              (if sdir
                (let ((source (resolve-resume-session-file parsed sdir)))
                  (if source (read-session-history source) '())
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
               (emit-initial? #t)
             ) ;run-agent-loop-with-timeout
      (let* ((timeout (pi-resolve-timeout parsed cfg))
             (max-turns
               (or (pi-args-ref parsed 'max-turns)
                 (cdr (or (assoc 'max-turns cfg) (cons #f 0)))
               ) ;or
             ) ;max-turns
             (session-timeout
               (let ((st (pi-args-ref parsed 'session-timeout)))
                 (if (number? st) st (cdr (or (assoc 'session-timeout cfg) '(#f
                                                                             . 0))))
               ) ;let
             ) ;session-timeout
            ) ;
        (run-agent-loop cfg init-msgs system-prompt emit timeout max-turns
          session-timeout compaction pi-request-provider-stream emit-initial?
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

    ;; 恢复场景的上下文由投影入口统一产出（1205）
    (define (%project-restore-context sfile)
      (let*-values (((msgs _state diag)
                     (pi-build-session-projection (pi-read-session-entries sfile))
                    ) ;
                   ) ;
        (%warn-projection-diagnostics diag)
        msgs
      ) ;let*-values
    ) ;define

    ;; 投影诊断非空时写 stderr，与压缩摘要输出通道一致，不吞掉降级原因
    (define (%warn-projection-diagnostics diag)
      (for-each
        (lambda (p)
          (let ((reason (cdr p)))
            (cond
             ((string? reason)
              (display-stderr (string-append "projection warning: " (symbol->string (car p)) ": " reason)
              ) ;display-stderr
             ) ;
             ((pair? reason)
              (display-stderr (string-append "projection warning: "
                                (symbol->string (car p))
                                ": skipped "
                                (number->string (length reason))
                                " unknown-type entries"
                              ) ;string-append
              ) ;display-stderr
             ) ;
             (else #f)
            ) ;cond
          ) ;let
        ) ;lambda
        (or diag '())
      ) ;for-each
    ) ;define

    (define (%session-context parsed sfile sdir session-file-override)
      ;; 会话文件覆盖（TUI worker 复用同一文件）时始终携带既有上下文；
      ;; 否则 --continue / --resume 时恢复历史
      (if
        (and sfile
          (or session-file-override (and sdir (resolve-resume-session-file parsed sdir)))
        ) ;and
        (%project-restore-context sfile)
        '()
      ) ;if
    ) ;define

    ;; session-file（可选）：TUI 后台 worker 传入复用的会话文件，
    ;; 同一 TUI 运行内跨消息保持上下文连续（对齐 pi 单会话模型）
    (define* (run-session-flow parsed
               cfg
               initial-msg
               emit
               on-finish
               (session-file #f)
               (rest-msgs '())
               (emit-initial? #t)
             ) ;run-session-flow
      (let*-values (((cwd) (path->string (path-cwd)))
                    ((sfile sdir)
                     (if session-file
                       (values session-file #f)
                       (setup-session-file-with-name parsed cfg cwd)
                     ) ;if
                    ) ;
                    ((context) (%session-context parsed sfile sdir session-file))
                   ) ;
        (let ((prompts (append (if (string-null? initial-msg) '() (list initial-msg)) rest-msgs)
              ) ;prompts
             ) ;
          (when emit
            (emit 'session cwd)
          ) ;when
          (let ((state (vector context (length context)))
                (timeout (pi-resolve-timeout parsed cfg))
               ) ;
            (let loop
              ((remaining prompts) (current-msgs context))
              (let* ((next-msgs
                       (if (pair? remaining)
                         (append current-msgs (list (pi-user-message (car remaining))))
                         current-msgs
                       ) ;if
                     ) ;next-msgs
                    ) ;
                (when (pair? remaining)
                  (persist-new-messages! sfile current-msgs next-msgs (length current-msgs))
                ) ;when
                (vector-set! state 0 next-msgs)
                (vector-set! state 1 (length next-msgs))
                (let* ((compaction (pi-make-compaction-option parsed cfg sfile state timeout))
                       (res-msgs (run-agent-loop-with-timeout cfg
                                   next-msgs
                                   (%system-prompt-for cfg parsed)
                                   emit
                                   parsed
                                   compaction
                                   emit-initial?
                                 ) ;run-agent-loop-with-timeout
                       ) ;res-msgs
                      ) ;
                  (persist-new-messages! sfile (vector-ref state 0) res-msgs (vector-ref state 1))
                  (if (and (pair? remaining) (pair? (cdr remaining)))
                    (begin
                      (vector-set! state 0 res-msgs)
                      (vector-set! state 1 (length res-msgs))
                      (loop (cdr remaining) res-msgs)
                    ) ;begin
                    (on-finish res-msgs)
                  ) ;if
                ) ;let*
              ) ;let*
            ) ;let
          ) ;let
        ) ;let
      ) ;let*-values
    ) ;define*

    (define (run-compact-flow parsed cfg initial-msg emit)
      (let* ((cwd (path->string (path-cwd)))
             (agent-dir (cdr (assoc 'agent-dir cfg)))
             (sdir (resolve-session-dir parsed cfg cwd))
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
              ;; 压缩写入条目后重新投影，投影结果之后才追加本次用户消息
              (let* ((projection (%project-restore-context sfile))
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
