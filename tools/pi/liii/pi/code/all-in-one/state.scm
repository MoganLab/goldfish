;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 进程级调度状态：链到粘性下标的映射、节点身份、节点最近故障
;; 时间戳与冷却窗口判断。只有两个映射需要跨调用保留，因此放在独立的平铺
;; 模块中；调度算法本身（选择、切换、探活、结算）在 scheduler。
(define-library (liii pi code all-in-one state)
  (import (scheme base)
    (liii list)
    (liii string)
    (liii date-time)
    (liii pi code all-in-one types)
  ) ;import
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

    ;; 状态容器不变量：粘性下标为 ((chain-id . index) ...)，节点最近故障为
    ;; ((node-key . timestamp-ms) ...)；两者都是进程级、跨请求保留的单表，
    ;; 同一 key 只保留最新一条记录。
    (define %last-success-index '())

    (define %node-failure-timestamps '())

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

    (define (%check-chain-id proc-name chain-id)
      (unless (string? chain-id)
        (error 'type-error
          (string-append proc-name ": chain-id must be a string")
          chain-id
        ) ;error
      ) ;unless
    ) ;define

    (define (pi-chain-clear-sticky-node! chain-id)
      (%check-chain-id "pi-chain-clear-sticky-node!" chain-id)
      (%clear-success! chain-id)
    ) ;define

    (define (pi-chain-record-sticky-node! chain-id index)
      (%check-chain-id "pi-chain-record-sticky-node!" chain-id)
      (unless (and (integer? index) (>= index 0))
        (error 'type-error
          "pi-chain-record-sticky-node!: index must be a non-negative integer"
          index
        ) ;error
      ) ;unless
      (%remember-success! chain-id index)
    ) ;define

    (define (pi-chain-current-sticky-index chain-id)
      (%check-chain-id "pi-chain-current-sticky-index" chain-id)
      (%previous-success-index chain-id)
    ) ;define

    ;; 节点身份键不变量：provider/model:thinkingLevel 三元组唯一标识一个能力
    ;; 组合；日志标签（provider/model）是日志格式关注点，由 scheduler 自己生成。
    (define (%node-key node)
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

    ;; 时间注入：调用方可传毫秒数或无参过程，缺省用系统单调时钟。
    (define (%current-now now-val)
      (cond ((procedure? now-val) (now-val))
            ((number? now-val) now-val)
            (else (current-monotonic-millis))
      ) ;cond
    ) ;define

    (define (%record-node-failure! node now-ms)
      (let ((key (%node-key node)))
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
      (let ((key (%node-key node)))
        (set! %node-failure-timestamps
          (filter
            (lambda (p) (not (equal? (car p) key)))
            %node-failure-timestamps
          ) ;filter
        ) ;set!
      ) ;let
    ) ;define

    (define (%node-last-failure node)
      (let ((p (assoc (%node-key node) %node-failure-timestamps)))
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

    ;; 冷却窗口：cooldown-ms <= 0 视为禁用冷却；故障时间戳在未来（elapsed 为负）
    ;; 时按未冷却处理，避免时钟回拨把节点永久钉在冷却期。
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

  ) ;begin
) ;define-library
