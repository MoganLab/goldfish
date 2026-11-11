;; pi-chain-reset-sticky-state!
;; 清空各链当前粘性节点记忆，使下一次调度重新从链头探活。
;;
;; 语法
;; ----
;; (pi-chain-reset-sticky-state!)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; unspecified
;; 清空粘性状态，不改变链的优先级顺序。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-sticky-reset-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define N1 (make-chain-node "p1" "m1"))
(define N2 (make-chain-node "p2" "m2"))
(define CHAIN (make-chain-def "sticky-reset-test" "粘性重置" (list N1 N2)))
(define CFG (make-all-in-one-cfg #f 1000 10 1000 (list CHAIN)))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

(define (execute-with-probe probe-fn)
  (pi-chain-execute CHAIN '() (lambda (provider . args) provider) CFG TMP-DIR #f probe-fn)
) ;define

;; 先切换到 p2，再确认下一轮只探测并复用 p2。
(check
  (execute-with-probe
    (lambda (node . args) (equal? (chain-node-provider node) "p2"))
  ) ;execute-with-probe
  =>
  "p2"
) ;check
(let ((probed '()))
  (check
    (execute-with-probe
      (lambda (node . args)
        (set! probed (cons (chain-node-provider node) probed))
        #t
      ) ;lambda
    ) ;execute-with-probe
    =>
    "p2"
  ) ;check
  (check (reverse probed) => '("p2"))
) ;let

;; 重置后不再粘性复用 p2，下一轮从链头 p1 开始。
(pi-chain-reset-sticky-state!)
(let ((probed '()))
  (check
    (execute-with-probe
      (lambda (node . args)
        (set! probed (cons (chain-node-provider node) probed))
        #t
      ) ;lambda
    ) ;execute-with-probe
    =>
    "p1"
  ) ;check
  (check (reverse probed) => '("p1"))
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
