(import (liii check) (scheme base))
(check-set-mode! 'report-failed)

;; truncate-remainder
;; 向零截断的整数除法余数。
;;
;; 语法
;; ----
;; (truncate-remainder n1 n2)
;;
;; 返回值
;; ----
;; integer?

(check (truncate-remainder 7 2) => 1)
(check (truncate-remainder -7 2) => -1)
(check (truncate-remainder 7 -2) => 1)
(check (truncate-remainder -7 -2) => -1)

;; 满足 n1 = (truncate-quotient n1 n2) * n2 + (truncate-remainder n1 n2)
(check (= 7 (+ (* (truncate-quotient 7 2) 2) (truncate-remainder 7 2))) => #t)
(check (= -7 (+ (* (truncate-quotient -7 2) 2) (truncate-remainder -7 2))) => #t)

(check-report)
