(import (liii check) (scheme lazy))
(check-set-mode! 'report-failed)

;; delay-force
;; 创建惰性 promise，其值本身是一个 promise。
;;
;; 语法
;; ----
;; (delay-force expression)
;;
;; 返回值
;; ----
;; promise

(define lazy-df (delay-force (delay 7)))
(check (force lazy-df) => 7)

(check-report)
