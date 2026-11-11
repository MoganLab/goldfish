;; pi-chain-reset-rewind-state!
;; 清空用于 REWIND 日志判定的上一成功节点记忆。
;;
;; 语法
;; ----
;; (pi-chain-reset-rewind-state!)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; unspecified
;; 清空记忆，不影响节点选择顺序。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code all-in-one chain)
  (liii pi code all-in-one types)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "aio-chain-reset-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define
(define N1 (make-chain-node "p1" "m1"))
(define N2 (make-chain-node "p2" "m2"))
(define CHAIN (make-chain-def "reset-test" "重置测试" (list N1 N2)))
(define CFG (make-all-in-one-cfg #f 1000 10 1000 (list CHAIN)))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

(define (run-on-node-2)
  (pi-chain-execute CHAIN '() (lambda args 'node-2) CFG TMP-DIR #f
    (lambda (node timeout config-dir run-fn)
      (equal? (chain-node-provider node) "p2")
    ) ;lambda
  ) ;pi-chain-execute
) ;define

;; 节点 2 成功后立即回链头本应产生 REWIND；重置记忆后则不再记录。
(check (run-on-node-2) => 'node-2)
(pi-chain-reset-rewind-state!)
(check (pi-chain-execute CHAIN '() (lambda args 'node-1) CFG TMP-DIR #f (lambda args #t))
  => 'node-1
) ;check
(let ((content (path-read-text (path->string (path-join TMP-DIR "alarm.log")))))
  (check (string-contains? content "[REWIND]") => #f)
  (check-true (string-contains? content "[SWITCH]"))
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
