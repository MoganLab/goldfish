;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 调度算法：正常选择、故障切换、探活、真实执行、候选管理与日志。
;; 进程级粘性/冷却状态由 (liii pi code all-in-one state) 持有，本模块只读写其
;; 公开接口，因此不保存任何跨调用状态。
(define-library (liii pi code all-in-one scheduler)
  (import (scheme base)
    (liii list)
    (liii string)
    (liii path)
    (liii date-time)
    (liii pi code all-in-one alarm)
    (liii pi code all-in-one boundary)
    (liii pi code all-in-one probe)
    (liii pi code all-in-one state)
    (liii pi code all-in-one types)
    (liii pi code config)
  ) ;import
  ;; 调度
  (export pi-chain-execute)
  ;; 全链结算
  (export pi-chain-settle-exhausted!)
  (begin

    ;; 日志标签不变量：只用 provider/model，等级不进入日志文本。
    (define (%node-label node)
      (string-append (chain-node-provider node) "/" (chain-node-model node))
    ) ;define

    (define (%config-dir-str config-dir)
      (let ((dir (or config-dir (gf-get-config-dir))))
        (if (string? dir) dir (path->string dir))
      ) ;let
    ) ;define

    (define (%log-handoff chain-id index cand previous-index node-total dir)
      (let ((cand-label (%node-label cand)))
        (when (and previous-index (> previous-index index))
          (pi-alarm-log! "REWIND"
            (chain-node-provider cand)
            (chain-node-model cand)
            'probe-ok
            (string-append "chain="
              chain-id
              " recovered="
              cand-label
              " index="
              (number->string (+ index 1))
              " probe=ok"
            ) ;string-append
            dir
          ) ;pi-alarm-log!
        ) ;when
        (when
          (and (> index 0)
            (not (eqv? previous-index index))
            (not (and previous-index (> previous-index index)))
          ) ;and
          (pi-alarm-log! "SWITCH"
            (chain-node-provider cand)
            (chain-node-model cand)
            'probe-ok
            (string-append "chain="
              chain-id
              " index="
              (number->string (+ index 1))
              "/"
              (number->string node-total)
              " probe=ok"
            ) ;string-append
            dir
          ) ;pi-alarm-log!
        ) ;when
      ) ;let
    ) ;define

    ;; 全链结算不变量：写一条 [EXHAUSTED] 日志、清除该链粘性，并返回 exhausted
    ;; 哨兵。调度内部无候选可用与调用方用尽本轮真实请求预算都走这里，保证两条
    ;; 路径的日志、粘性与返回值完全一致。
    (define* (pi-chain-settle-exhausted! chain-def (config-dir #f))
      (unless (chain-def? chain-def)
        (error 'type-error
          "pi-chain-settle-exhausted!: chain-def must be a <chain-def>"
          chain-def
        ) ;error
      ) ;unless
      (let* ((chain-id (chain-def-id chain-def))
             (node-total (length (chain-def-nodes chain-def)))
             (dir (%config-dir-str config-dir))
            ) ;
        (pi-alarm-log! "EXHAUSTED"
          "all"
          chain-id
          0
          (string-append "All "
            (number->string node-total)
            " nodes in chain failed probe"
          ) ;string-append
          dir
        ) ;pi-alarm-log!
        (pi-chain-clear-sticky-node! chain-id)
        'all-nodes-exhausted
      ) ;let*
    ) ;define*

    ;; 调度分三段：正常路径（粘性复用或首个未冷却节点，直接真实请求，不探活）、
    ;; 故障切换路径（真实失败后才探活候选节点）、全链结算（返回 exhausted 哨兵）。
    (define* (pi-chain-execute chain-def
               messages
               user-fn
               cfg
               (config-dir #f)
               (request-fn #f)
               (probe-fn #f)
               (skip-index #f)
               (now-fn #f)
             ) ;pi-chain-execute
      (unless (chain-def? chain-def)
        (error 'type-error
          "pi-chain-execute: chain-def must be a <chain-def>"
          chain-def
        ) ;error
      ) ;unless
      (unless (all-in-one-cfg? cfg)
        (error 'type-error "pi-chain-execute: cfg must be an <all-in-one-cfg>" cfg)
      ) ;unless
      (unless (procedure? user-fn)
        (error 'type-error "pi-chain-execute: user-fn must be a procedure" user-fn)
      ) ;unless
      (unless (list? messages)
        (error 'type-error "pi-chain-execute: messages must be a list" messages)
      ) ;unless
      (let* ((timeout-ms (all-in-one-cfg-probe-timeout-ms cfg))
             (cooldown-ms
               (let ((r (all-in-one-cfg-retry-interval-ms cfg)))
                 (if (and (number? r) (>= r 0)) r 60000)
               ) ;let
             ) ;cooldown-ms
             (dir (%config-dir-str config-dir))
             (probe (or probe-fn pi-probe-node))
             (nodes (chain-def-nodes chain-def))
             (node-total (length nodes))
             (chain-id (chain-def-id chain-def))
             (previous-index (pi-chain-current-sticky-index chain-id))
            ) ;
        ;; 正常路径与切换路径共用的交接动作：边界清洗 -> 记录粘性 -> 清除候选故障
        (define (execute-candidate cand index)
          (let ((clean-msgs (pi-boundary-recover-messages messages)))
            (%log-handoff chain-id index cand previous-index node-total dir)
            (pi-chain-record-sticky-node! chain-id index)
            (pi-chain-clear-node-failure! cand)
            (user-fn (chain-node-provider cand)
              (chain-node-model cand)
              (chain-node-thinking-level cand)
              clean-msgs
            ) ;user-fn
          ) ;let
        ) ;define

        ;; 切换路径：从链头扫描，跳过刚失败下标与冷却期节点，命中即探活
        (define (probe-from-head skip-index)
          (let loop
            ((rest nodes) (index 0))
            (cond ((null? rest) (pi-chain-settle-exhausted! chain-def dir))
                  ((eqv? index skip-index) (loop (cdr rest) (+ index 1)))
                  (else
                    (let ((cand (car rest)))
                      (if (pi-chain-node-in-cooldown? cand cooldown-ms now-fn)
                        (loop (cdr rest) (+ index 1))
                        (if (probe cand timeout-ms dir request-fn)
                          (execute-candidate cand index)
                          (begin
                            (pi-chain-record-node-failure! cand now-fn)
                            (loop (cdr rest) (+ index 1))
                          ) ;begin
                        ) ;if
                      ) ;if
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;define

        ;; 正常路径：从链头挑首个未冷却节点直接执行，不探活
        (define (execute-first-available)
          (let loop
            ((rest nodes) (index 0))
            (if (null? rest)
              (pi-chain-settle-exhausted! chain-def dir)
              (let ((cand (car rest)))
                (if (pi-chain-node-in-cooldown? cand cooldown-ms now-fn)
                  (loop (cdr rest) (+ index 1))
                  (execute-candidate cand index)
                ) ;if
              ) ;let
            ) ;if
          ) ;let
        ) ;define

        (cond (skip-index (probe-from-head skip-index))
              ((and previous-index
                 (integer? previous-index)
                 (>= previous-index 0)
                 (< previous-index node-total)
               ) ;and
               (execute-candidate (list-ref nodes previous-index) previous-index)
              ) ;
              (else (execute-first-available))
        ) ;cond
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
