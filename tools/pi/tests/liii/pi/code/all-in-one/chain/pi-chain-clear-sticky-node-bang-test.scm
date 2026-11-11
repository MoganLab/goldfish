;; pi-chain-clear-sticky-node!
;; 清除指定链的粘性节点记忆，不影响其他链。
;;
;; 语法
;; ----
;; (pi-chain-clear-sticky-node! chain-id)
;;
;; 参数
;; ----
;; chain-id : string
;; 要清除粘性节点的链标识。
;;
;; 返回值
;; ----
;; unspecified
;; 指定链下一次调度从链头探活。

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
      (string-append "aio-sticky-clear-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define N1 (make-chain-node "p1" "m1"))
(define N2 (make-chain-node "p2" "m2"))
(define CHAIN (make-chain-def "sticky-clear-test" "粘性清除" (list N1 N2)))
(define CFG (make-all-in-one-cfg #f 1000 10 1000 (list CHAIN)))

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

;; 先选中 p2，清除该链粘性后下一轮应从 p1 开始。
(check
  (pi-chain-execute CHAIN
    '()
    (lambda (provider . args) provider)
    CFG
    TMP-DIR
    #f
    (lambda (node . args) (equal? (chain-node-provider node) "p2"))
  ) ;pi-chain-execute
  =>
  "p2"
) ;check
(pi-chain-clear-sticky-node! "sticky-clear-test")
(let ((probed '()))
  (check
    (pi-chain-execute CHAIN
      '()
      (lambda (provider . args) provider)
      CFG
      TMP-DIR
      #f
      (lambda (node . args)
        (set! probed (cons (chain-node-provider node) probed))
        #t
      ) ;lambda
    ) ;pi-chain-execute
    =>
    "p1"
  ) ;check
  (check (reverse probed) => '("p1"))
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
