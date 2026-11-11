;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 每轮从链头顺次预检；首个健康节点清洗边界后执行真实业务。
;; 无持久禁用状态，主力恢复自然回溯；全挂返回 all-nodes-exhausted。
(define-library (liii pi code all-in-one chain)
  (import (scheme base)
    (liii list)
    (liii string)
    (liii path)
    (liii pi code all-in-one alarm)
    (liii pi code all-in-one boundary)
    (liii pi code all-in-one probe)
    (liii pi code all-in-one types)
    (liii pi code config)
  ) ;import
  (export pi-chain-execute pi-chain-node-label pi-chain-reset-rewind-state!)
  (begin

    ;; 仅用于 REWIND 日志，不参与节点选择。
    (define %last-success-index '())

    (define (pi-chain-reset-rewind-state!)
      (set! %last-success-index '())
    ) ;define

    (define (%remember-success! chain-id index)
      (set! %last-success-index
        (cons (cons chain-id index)
          (filter
            (lambda (p) (not (equal? (car p) chain-id)))
            %last-success-index
          ) ;filter
        ) ;cons
      ) ;set!
    ) ;define

    (define (%previous-success-index chain-id)
      (let ((p (assoc chain-id %last-success-index)))
        (if p (cdr p) #f)
      ) ;let
    ) ;define

    (define (pi-chain-node-label node)
      (string-append (chain-node-provider node) "/" (chain-node-model node))
    ) ;define

    (define (%config-dir-str config-dir)
      (let ((dir (or config-dir (gf-get-config-dir))))
        (if (string? dir) dir (path->string dir))
      ) ;let
    ) ;define

    (define (%log-handoff chain-id index cand previous-index node-total dir)
      (let ((cand-label (pi-chain-node-label cand)))
        (when (> index 0)
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
      ) ;let
    ) ;define

    ;; user-fn 接收 provider、model、thinking-level 和 clean-messages。
    ;; config-dir/run-fn/probe-fn 可选，后两者用于隔离测试。
    (define* (pi-chain-execute chain-def
               messages
               user-fn
               cfg
               (config-dir #f)
               (run-fn #f)
               (probe-fn #f)
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
             (dir (%config-dir-str config-dir))
             (probe (or probe-fn pi-probe-node))
             (nodes (chain-def-nodes chain-def))
             (node-total (length nodes))
             (chain-id (chain-def-id chain-def))
             (previous-index (%previous-success-index chain-id))
            ) ;
        (let loop
          ((rest nodes) (index 0))
          (if (null? rest)
            (begin
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
              'all-nodes-exhausted
            ) ;begin
            (let ((cand (car rest)))
              (if (probe cand timeout-ms dir run-fn)
                ;; 只在真实请求前清洗一次边界
                (let ((clean-msgs (pi-boundary-recover-messages messages)))
                  (%log-handoff chain-id index cand previous-index node-total dir)
                  (%remember-success! chain-id index)
                  (user-fn (chain-node-provider cand)
                    (chain-node-model cand)
                    (chain-node-thinking-level cand)
                    clean-msgs
                  ) ;user-fn
                ) ;let
                (loop (cdr rest) (+ index 1))
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
