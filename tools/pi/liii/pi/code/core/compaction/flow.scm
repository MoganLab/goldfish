;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code core compaction flow)
  (import (scheme base)
    (scheme write)
    (liii list)
    (liii date-time)
    (liii string)
    (liii pi ai config record)
    (liii pi ai models)
    (liii pi ai models meta)
    (liii pi ai providers registry)
    (liii pi code cli args)
    (liii pi code config)
    (liii pi code core session-manager)
    (liii pi code core compaction tokens)
    (liii pi code core compaction session)
    (liii pi code core session-projection)
    (liii pi code all-in-one types)
    (liii pi code all-in-one loader)
    (liii pi code all-in-one chain)
    (liii pi code all-in-one provider)
  ) ;import
  (export pi-format-compaction-summary
    pi-make-compaction-option
    pi-resolve-compaction-window
  ) ;export
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

    ;; 解析链中单节点的上下文窗口（模型库优先，回退内置目录与兜底）
    (define (%node-compaction-window node store)
      (let* ((p (chain-node-provider node))
             (m (chain-node-model node))
             (meta
               (and store
                 (catch #t (lambda () (pi-resolve-model-metadata p m store)) (lambda args #f))
               ) ;and
             ) ;meta
            ) ;
        (cond
         ((and meta
            (pi-model-metadata? meta)
            (number? (pi-model-metadata-context-window meta))
          ) ;and
          (pi-model-metadata-context-window meta)
         ) ;
         (else
           (let* ((p-models (cdr (or (assoc p PI_PROVIDER_BUILTIN_MODELS) '(#f))))
                  (entry
                    (find
                      (lambda (e) (equal? (cdr (or (assoc "id" e) '(#f . #f))) m))
                      p-models
                    ) ;find
                  ) ;entry
                 ) ;
             (if entry (cdr (or (assoc "contextWindow" entry) '(#f . 1000000))) 1000000)
           ) ;let*
         ) ;else
        ) ;cond
      ) ;let*
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

    ;; 动态计算本次请求前用于自动压缩判定（%auto-compact）的上下文容量：
    ;; 对于 all-in-one 伪模型：
    ;;   1. 若当前链路有已就绪的有效粘性节点（且未处于冷却期），使用该实际节点的上下文窗口；
    ;;   2. 若尚未建立粘性、或粘性节点已进入冷却期（故障被跳过），使用整条链路候选节点的保守窗口（最小值）；
    ;;   3. 杜绝简单硬编码 1M 或沿用过期/故障节点的窗口大小。
    ;; 对于常规供应商模型：直接使用当前模型的 metadata contextWindow。
    (define (pi-resolve-compaction-window cfg)
      (let ((p (cdr (or (assoc 'provider cfg) '(#f . #f)))))
        (if (not (equal? p "all-in-one"))
          (%context-window cfg)
          (let* ((dir (pi-cfg-config-dir cfg))
                 (raw-aio (all-in-one-load-config dir))
                 (default-chains (all-in-one-default-chains))
                 (aio-cfg (or raw-aio (make-all-in-one-cfg #t 3600000 1800000 50000 default-chains))
                 ) ;aio-cfg
                 (chains (all-in-one-cfg-chains aio-cfg))
                 (chain-id (cdr (or (assoc 'model-id cfg) '(#f . #f))))
                 (chain
                   (and chain-id
                     (or
                       (find (lambda (c) (equal? (chain-def-id c) chain-id)) chains)
                       (and (or (equal? chain-id "coding-deep") (equal? chain-id "coding-high-level"))
                         (find (lambda (c) (equal? (chain-def-id c) "coding-high-quality")) chains)
                       ) ;and
                       (and (equal? chain-id "coding-fast-cheap")
                         (find (lambda (c) (equal? (chain-def-id c) "coding-fast")) chains)
                       ) ;and
                       (find (lambda (c) (equal? (chain-def-id c) chain-id)) default-chains)
                     ) ;or
                   ) ;and
                 ) ;chain
                 (nodes (if chain (chain-def-nodes chain) '()))
                 (store (cdr (or (assoc 'models-store cfg) '(#f . #f))))
                 (conservative-window
                   (if (pair? nodes)
                     (apply min (map (lambda (n) (%node-compaction-window n store)) nodes))
                     (%context-window cfg)
                   ) ;if
                 ) ;conservative-window
                 (sticky-idx (and chain-id (pi-chain-current-sticky-index chain-id)))
                 (now-ms (current-monotonic-millis))
                ) ;
            (cond
             ((and (integer? sticky-idx)
                (>= sticky-idx 0)
                (< sticky-idx (length nodes))
                (not (pi-chain-node-in-cooldown? (list-ref nodes sticky-idx) now-ms aio-cfg))
              ) ;and
              (%node-compaction-window (list-ref nodes sticky-idx) store)
             ) ;
             (else conservative-window)
            ) ;cond
          ) ;let*
        ) ;if
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
             (emit (if (pair? opt-emit) (car opt-emit) #f))
            ) ;
        (and enabled
          (list
            (cons 'compactor
              (lambda (msgs)
                (let ((window (pi-resolve-compaction-window cfg)))
                  (%auto-compact cfg sfile state timeout settings window msgs emit)
                ) ;let
              ) ;lambda
            ) ;cons
          ) ;list
        ) ;and
      ) ;let*
    ) ;define
  ) ;begin
) ;define-library
