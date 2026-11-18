;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one chain)
  (import (scheme base)
    (liii list)
    (liii string)
    (liii path)
    (liii date-time)
    (liii pi code all-in-one alarm)
    (liii pi code all-in-one boundary)
    (liii pi code all-in-one probe)
    (liii pi code all-in-one types)
    (liii pi code config)
  ) ;import
  ;; 调度
  (export pi-chain-execute)
  ;; 粘性节点状态
  (export pi-chain-current-sticky-index
    pi-chain-record-sticky-node!
    pi-chain-clear-sticky-node!
  ) ;export
  ;; 节点冷却状态
  (export pi-chain-record-node-failure! pi-chain-clear-node-failure!
    pi-chain-node-last-failure pi-chain-node-in-cooldown?
  ) ;export
  ;; 状态重置（测试清理与场景测试统一使用）
  (export pi-chain-reset-state! pi-chain-reset-cooldown-state!)
  (begin

    ;; 进程级调度状态：粘性下标（((chain-id . index) ...)）与
    ;; 节点最近故障时间戳（((node-key . timestamp-ms) ...)，单调毫秒）。
    (define %last-success-index '())

    (define %node-failure-timestamps '())

    ;; 同时清空粘性记忆与节点冷却，供场景测试回到初始状态。
    (define (pi-chain-reset-state!)
      (set! %last-success-index '())
      (set! %node-failure-timestamps '())
    ) ;define

    (define (pi-chain-reset-cooldown-state!)
      (set! %node-failure-timestamps '())
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

    (define (%clear-success! chain-id)
      (set! %last-success-index
        (filter
          (lambda (p) (not (equal? (car p) chain-id)))
          %last-success-index
        ) ;filter
      ) ;set!
    ) ;define

    (define (%previous-success-index chain-id)
      (let ((p (assoc chain-id %last-success-index)))
        (if p (cdr p) #f)
      ) ;let
    ) ;define

    (define (pi-chain-clear-sticky-node! chain-id)
      (%clear-success! chain-id)
    ) ;define

    (define (pi-chain-record-sticky-node! chain-id index)
      (%remember-success! chain-id index)
    ) ;define

    (define (pi-chain-current-sticky-index chain-id)
      (%previous-success-index chain-id)
    ) ;define

    (define (pi-chain-node-label node)
      (string-append (chain-node-provider node) "/" (chain-node-model node))
    ) ;define

    (define (pi-chain-node-key node)
      (cond ((chain-node? node)
             (string-append (chain-node-provider node)
               "/"
               (chain-node-model node)
               ":"
               (chain-node-thinking-level node)
             ) ;string-append
            ) ;
            ((string? node) node)
            (else (object->string node))
      ) ;cond
    ) ;define

    (define (%current-now now-val)
      (cond ((procedure? now-val) (now-val))
            ((number? now-val) now-val)
            (else (current-monotonic-millis))
      ) ;cond
    ) ;define

    (define (%record-node-failure! node now-ms)
      (let ((key (pi-chain-node-key node)))
        (set! %node-failure-timestamps
          (cons (cons key now-ms)
            (filter
              (lambda (p) (not (equal? (car p) key)))
              %node-failure-timestamps
            ) ;filter
          ) ;cons
        ) ;set!
      ) ;let
    ) ;define

    (define (%clear-node-failure! node)
      (let ((key (pi-chain-node-key node)))
        (set! %node-failure-timestamps
          (filter
            (lambda (p) (not (equal? (car p) key)))
            %node-failure-timestamps
          ) ;filter
        ) ;set!
      ) ;let
    ) ;define

    (define (%node-last-failure node)
      (let ((p (assoc (pi-chain-node-key node) %node-failure-timestamps)))
        (if p (cdr p) #f)
      ) ;let
    ) ;define

    (define (pi-chain-node-last-failure node)
      (%node-last-failure node)
    ) ;define

    (define* (pi-chain-record-node-failure! node (timestamp-ms #f))
      (let ((ts (%current-now timestamp-ms)))
        (%record-node-failure! node ts)
      ) ;let
    ) ;define*

    (define (pi-chain-clear-node-failure! node)
      (%clear-node-failure! node)
    ) ;define

    (define* (pi-chain-node-in-cooldown? node cooldown-ms (now-ms #f))
      (let ((last-fail (%node-last-failure node)) (now (%current-now now-ms)))
        (and (number? last-fail)
          (number? cooldown-ms)
          (> cooldown-ms 0)
          (let ((elapsed (- now last-fail)))
            (and (>= elapsed 0) (< elapsed cooldown-ms))
          ) ;let
        ) ;and
      ) ;let
    ) ;define*

    (define (%config-dir-str config-dir)
      (let ((dir (or config-dir (gf-get-config-dir))))
        (if (string? dir) dir (path->string dir))
      ) ;let
    ) ;define

    (define (%log-handoff chain-id index cand previous-index node-total dir)
      (let ((cand-label (pi-chain-node-label cand)))
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
             (previous-index (%previous-success-index chain-id))
            ) ;
        ;; 正常路径与切换路径共用的交接动作：边界清洗 -> 记录粘性 -> 清除候选故障
        (define (execute-candidate cand index)
          (let ((clean-msgs (pi-boundary-recover-messages messages)))
            (%log-handoff chain-id index cand previous-index node-total dir)
            (%remember-success! chain-id index)
            (%clear-node-failure! cand)
            (user-fn (chain-node-provider cand)
              (chain-node-model cand)
              (chain-node-thinking-level cand)
              clean-msgs
            ) ;user-fn
          ) ;let
        ) ;define

        ;; 全链结算：写 [EXHAUSTED] 日志并清除粘性
        (define (log-exhausted)
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
          (%clear-success! chain-id)
          'all-nodes-exhausted
        ) ;define

        ;; 切换路径：从链头扫描，跳过刚失败下标与冷却期节点，命中即探活
        (define (probe-from-head skip-index)
          (let loop
            ((rest nodes) (index 0))
            (cond ((null? rest) (log-exhausted))
                  ((eqv? index skip-index) (loop (cdr rest) (+ index 1)))
                  (else
                    (let ((cand (car rest)))
                      (if (pi-chain-node-in-cooldown? cand cooldown-ms (%current-now now-fn))
                        (loop (cdr rest) (+ index 1))
                        (if (probe cand timeout-ms dir request-fn)
                          (execute-candidate cand index)
                          (begin
                            (%record-node-failure! cand (%current-now now-fn))
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
              (log-exhausted)
              (let ((cand (car rest)))
                (if (pi-chain-node-in-cooldown? cand cooldown-ms (%current-now now-fn))
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
