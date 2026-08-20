(import (liii check) (scheme lazy))
(check-set-mode! 'report-failed)

;; delay
;; 创建惰性 promise。
;;
;; 语法
;; ----
;; (delay expression)
;;
;; 返回值
;; ----
;; promise
;; 首次 force 时求值 expression，之后返回缓存值。

(define lazy-x (delay (+ 1 2)))
(check (promise? lazy-x) => #t)
(check (force lazy-x) => 3)

(check-report)
