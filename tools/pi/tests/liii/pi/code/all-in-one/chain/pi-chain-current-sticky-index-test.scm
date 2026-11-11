;; pi-chain-current-sticky-index
;; 查询指定链当前粘性节点的下标，无记忆时返回 #f。
;;
;; 语法
;; ----
;; (pi-chain-current-sticky-index chain-id)
;;
;; 参数
;; ----
;; chain-id : string
;; 链标识。
;;
;; 返回值
;; ----
;; integer or #f
;; 当前粘性节点的零基下标；没有粘性节点时返回 #f。

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
      (string-append "aio-sticky-index-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define CHAIN
  (make-chain-def "sticky-index-test"
    "粘性下标"
    (list (make-chain-node "p1" "m1") (make-chain-node "p2" "m2"))
  ) ;make-chain-def
) ;define
(define CFG (make-all-in-one-cfg #f 1000 10 1000 (list CHAIN)))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
(pi-chain-reset-sticky-state!)

(check-false (pi-chain-current-sticky-index "sticky-index-test"))
(check
  (pi-chain-execute CHAIN
    '()
    (lambda args 'ok)
    CFG
    TMP-DIR
    #f
    (lambda (node . args) (equal? (chain-node-provider node) "p2"))
  ) ;pi-chain-execute
  =>
  'ok
) ;check
(check (pi-chain-current-sticky-index "sticky-index-test") => 1)
(pi-chain-clear-sticky-node! "sticky-index-test")
(check-false (pi-chain-current-sticky-index "sticky-index-test"))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
