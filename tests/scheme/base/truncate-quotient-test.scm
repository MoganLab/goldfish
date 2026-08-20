(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; truncate-quotient
;; 向零截断的整数除法商。
;;
;; 语法
;; ----
;; (truncate-quotient n1 n2)
;;
;; 返回值
;; ----
;; integer?

(check (truncate-quotient 7 2) => 3)
(check (truncate-quotient -7 2) => -3)
(check (truncate-quotient 7 -2) => -3)
(check (truncate-quotient -7 -2) => 3)
(check (truncate-quotient 10 3) => 3)
(check (truncate-quotient -10 3) => -3)

;; 与 floor-quotient 的区别：向零 vs 向下
(check (truncate-quotient -7 2) => -3)
(check (floor-quotient -7 2) => -4)

(check-report)
