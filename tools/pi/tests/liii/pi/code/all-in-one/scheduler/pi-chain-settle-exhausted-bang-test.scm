;; pi-chain-settle-exhausted!
;; 全链结算：写 EXHAUSTED 日志、清除该链粘性并返回 exhausted 哨兵。
;;
;; 语法
;; ----
;; (pi-chain-settle-exhausted! chain-def [config-dir])
;;
;; 参数
;; ----
;; chain-def : <chain-def>
;; 已用尽候选或已用尽本轮真实请求预算的链定义。
;; config-dir : string (可选)
;; 隔离的配置目录；提供时日志写入该目录下的 logs/。
;;
;; 返回值
;; ----
;; symbol
;; 恒为 all-nodes-exhausted。
;;
;; 说明
;; ----
;; 调度内部无候选可用与调用方用尽本轮真实请求预算共用本函数，因此两条路径的
;; EXHAUSTED 日志、粘性清除和返回值完全一致；日志 message 中的节点数是链长度。
;; 非 <chain-def> 入参抛 type-error。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii list)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one scheduler)
  (liii pi code all-in-one state)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-settle-exhausted-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define CHAIN
  (make-chain-def "settle-chain"
    "结算链"
    (list (make-chain-node "p1" "m1" "low")
      (make-chain-node "p2" "m2" "high")
      (make-chain-node "p3" "m3" "off")
    ) ;list
  ) ;make-chain-def
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
(pi-chain-reset-state!)

;; 1. 结算返回 exhausted 哨兵，清除粘性，并写一条带链长度的 EXHAUSTED 日志
(pi-chain-record-sticky-node! "settle-chain" 1)
(check (pi-chain-current-sticky-index "settle-chain") => 1)
(check (pi-chain-settle-exhausted! CHAIN TMP-DIR) => 'all-nodes-exhausted)
(check-false (pi-chain-current-sticky-index "settle-chain"))
(let* ((log-text (path-read-text (path->string (resolve-alarm-log-path TMP-DIR))))
       (exhausted-lines
         (filter
           (lambda (line) (string-contains? line "[EXHAUSTED]"))
           (string-split log-text "\n")
         ) ;filter
       ) ;exhausted-lines
      ) ;
  (check (length exhausted-lines) => 1)
  (check-true (string-contains? (car exhausted-lines) "model=settle-chain"))
  (check-true (string-contains? (car exhausted-lines) "All 3 nodes in chain failed probe"))
) ;let*

;; 2. 粘性只清除目标链，其他链的粘性保留
(pi-chain-record-sticky-node! "settle-chain" 2)
(pi-chain-record-sticky-node! "other-chain" 0)
(check (pi-chain-settle-exhausted! CHAIN TMP-DIR) => 'all-nodes-exhausted)
(check-false (pi-chain-current-sticky-index "settle-chain"))
(check (pi-chain-current-sticky-index "other-chain") => 0)

;; 3. 非 <chain-def> 入参明确抛 type-error
(check-catch 'type-error (pi-chain-settle-exhausted! #f TMP-DIR))
(check-catch 'type-error (pi-chain-settle-exhausted! "settle-chain" TMP-DIR))

(pi-chain-reset-state!)

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
