(import (liii check) (scheme lazy))
(check-set-mode! 'report-failed)

;; force
;; 强制求值 promise。
;;
;; 语法
;; ----
;; (force promise)
;;
;; 返回值
;; ----
;; 已求值的结果

(define evaluated 0)

(define lazy-once (delay (begin (set! evaluated (+ evaluated 1)) 42)))
(check evaluated => 0)
(check (force lazy-once) => 42)
(check evaluated => 1)
;; 再次 force 返回缓存值，不重新求值
(check (force lazy-once) => 42)
(check evaluated => 1)

(check-report)
