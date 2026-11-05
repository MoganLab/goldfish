;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core compaction flow)
  (import (scheme base)
    (scheme write)
    (liii string)
    (liii pi ai config record)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code core session-manager)
    (liii pi code core compaction tokens)
    (liii pi code core compaction session)
    (liii pi code core session-projection)
  ) ;import
  (export pi-format-compaction-summary pi-make-compaction-option)
  (begin

    (define (%display-stderr msg)
      (let ((p (if (defined? '*stderr*) *stderr* (current-error-port))))
        (display msg p)
        (newline p)
      ) ;let
    ) ;define
    ;; 投影诊断非空时写 stderr，与压缩摘要输出通道一致，不吞掉降级原因
    (define (%warn-projection-diagnostics diag)
      (for-each
        (lambda (p)
          (let ((reason (cdr p)))
            (cond
             ((string? reason)
              (%display-stderr (string-append "projection warning: " (symbol->string (car p)) ": " reason)
              ) ;%display-stderr
             ) ;
             ((pair? reason)
              (%display-stderr (string-append "projection warning: "
                                 (symbol->string (car p))
                                 ": skipped "
                                 (number->string (length reason))
                                 " unknown-type entries"
                               ) ;string-append
              ) ;%display-stderr
             ) ;
             (else #f)
            ) ;cond
          ) ;let
        ) ;lambda
        (or diag '())
      ) ;for-each
    ) ;define

    (define (pi-format-compaction-summary result . opt-label)
      (let ((label
              (if (and (pair? opt-label) (string? (car opt-label)))
                (car opt-label)
                "Compacted"
              ) ;if
            ) ;label
           ) ;
        (string-append label
          " session: summarized "
          (number->string (cdr (assoc 'summarized-count result)))
          " messages, kept "
          (number->string (cdr (assoc 'kept-count result)))
          ", tokens before "
          (number->string (cdr (assoc 'tokens-before result)))
        ) ;string-append
      ) ;let
    ) ;define

    ;; 当前模型的上下文窗口：model-meta 为 pi-model-metadata record，
    ;; 兼容 alist 形态（旧会话状态），record 须走访问器读取
    (define (%context-window cfg)
      (let ((meta (cdr (or (assoc 'model-meta cfg) '(#f)))))
        (cond ((pi-model-metadata? meta) (pi-model-metadata-context-window meta))
              ((pair? meta)
               (let ((pair (assoc 'context-window meta)))
                 (if pair (cdr pair) #f)
               ) ;let
              ) ;
              (else #f)
        ) ;cond
      ) ;let
    ) ;define

    (define (%auto-compact cfg sfile state timeout settings window msgs . opt-emit)
      (let* ((estimate (pi-context-token-estimate msgs))
             (tokens (pi-context-token-estimate-tokens estimate))
             (emit (if (pair? opt-emit) (car opt-emit) #f))
            ) ;
        (if (not (pi-should-compact? tokens window settings))
          msgs
          (begin
            (persist-new-messages! sfile (vector-ref state 0) msgs (vector-ref state 1))
            (vector-set! state 0 msgs)
            (vector-set! state 1 (length msgs))
            (let* ((result (pi-compact-session! cfg sfile settings timeout "auto")))
              (%display-stderr (pi-format-compaction-summary result "Auto-compacted"))
              ;; 上下文已重建：通知前端占比转为未知（对齐上游压缩后显示 "?"）
              (when (procedure? emit)
                (emit 'compaction)
              ) ;when
              ;; 压缩条目已落盘，上下文由投影入口统一重建（1206）
              (let*-values (((projection _pstate diag)
                             (pi-build-session-projection (pi-read-session-entries sfile))
                            ) ;
                           ) ;
                ;; 诊断非空只告警，不中断压缩流程
                (%warn-projection-diagnostics diag)
                (vector-set! state 0 projection)
                (vector-set! state 1 (length projection))
                projection
              ) ;let*-values
            ) ;let*
          ) ;begin
        ) ;if
      ) ;let*
    ) ;define

    (define (pi-make-compaction-option parsed cfg sfile state timeout . opt-emit)
      (let* ((config-dir (pi-cfg-config-dir cfg))
             (settings (pi-load-compaction-settings config-dir))
             (enabled (and sfile
                        (not (pi-args-ref parsed 'no-auto-compact))
                        (eq? #t (pi-compaction-settings-enabled settings))
                      ) ;and
             ) ;enabled
             (window (%context-window cfg))
             (emit (if (pair? opt-emit) (car opt-emit) #f))
            ) ;
        (and enabled
          (list (cons 'compactor
                  (lambda (msgs)
                    (%auto-compact cfg sfile state timeout settings window msgs emit)
                  ) ;lambda
                ) ;cons
          ) ;list
        ) ;and
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
