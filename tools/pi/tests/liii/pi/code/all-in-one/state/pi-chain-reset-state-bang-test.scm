;; pi-chain-reset-state!
;; 清空进程级调度状态（粘性下标与节点故障时间戳），使调度回到初始状态。
;;
;; 语法
;; ----
;; (pi-chain-reset-state!)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; unspecified
;; 粘性记忆与冷却记录都被清空；链定义与优先级顺序不受影响。
;;
;; 说明
;; ----
;; 场景测试用它回到初始状态。它同时清空粘性与冷却，因此不再提供
;; “只清粘性”或“只清 REWIND 记忆”的语义别名。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code all-in-one scheduler)
  (liii pi code all-in-one state)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-reset-state-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define N1 (make-chain-node "p1" "m1"))

(define N2 (make-chain-node "p2" "m2"))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

;; 1. 粘性与冷却被同时清空
(pi-chain-record-sticky-node! "reset-chain" 1)
(pi-chain-record-node-failure! N1 1000)
(check (pi-chain-current-sticky-index "reset-chain") => 1)
(check (pi-chain-node-last-failure N1) => 1000)

(pi-chain-reset-state!)

(check-false (pi-chain-current-sticky-index "reset-chain"))
(check-false (pi-chain-node-last-failure N1))
(check-false (pi-chain-node-in-cooldown? N1 60000 5000))

;; 2. 状态清空后调度回到链头，且不产生 REWIND
(let* ((chain (make-chain-def "reset-exec" "重置调度" (list N1 N2)))
       (cfg (make-all-in-one-cfg #f 1000 60000 1000 (list chain)))
       (run
         (lambda (user-result)
           (pi-chain-execute chain
             '()
             (lambda (provider . args) user-result)
             cfg
             TMP-DIR
             #f
             (lambda (node . args) (equal? (chain-node-provider node) "p2"))
             0
           ) ;pi-chain-execute
         ) ;lambda
       ) ;run
      ) ;
  ;; 先落到链尾 p2，再重置状态
  (check (run 'on-p2) => 'on-p2)
  (check (pi-chain-current-sticky-index "reset-exec") => 1)
  (pi-chain-reset-state!)
  (check (pi-chain-execute chain
           '()
           (lambda (provider . args) provider)
           cfg
           TMP-DIR
           #f
           (lambda args #t)
         ) ;pi-chain-execute
    =>
    "p1"
  ) ;check
) ;let*

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
