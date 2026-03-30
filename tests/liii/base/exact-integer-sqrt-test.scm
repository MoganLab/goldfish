(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; exact-integer-sqrt
;; 计算给定非负精确整数的精确平方根。
;;
;; 语法
;; ----
;; (exact-integer-sqrt n)
;;
;; 参数
;; ----
;; n : exact?
;; n是确切的非负整数。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：
;; 1. 整数r：满足r² ≤ n的最大整数
;; 2. 整数remainder：n - r²，始终为非负
;;
;; 说明
;; ----
;; 该函数专为精确计算设计，要求参数必须是非负的准确整数。
;; 对于完全平方数，remainder将为0；非完全平方数返回最大的整数根和余量。
;;
;; 错误处理
;; --------
;; type-error
;; 当参数不是准确的整数时抛出错误。
;; value-error
;; 当参数是负数时抛出错误。

(check (list (exact-integer-sqrt 9)) => (list 3 0))
(check (list (exact-integer-sqrt 5)) => (list 2 1))
(check (list (exact-integer-sqrt 0)) => (list 0 0))
(check (list (exact-integer-sqrt 1)) => (list 1 0))
(check (list (exact-integer-sqrt 4)) => (list 2 0))
(check (list (exact-integer-sqrt 16)) => (list 4 0))
(check (list (exact-integer-sqrt 2)) => (list 1 1))
(check (list (exact-integer-sqrt 3)) => (list 1 2))
(check (list (exact-integer-sqrt 8)) => (list 2 4))
(check (list (exact-integer-sqrt 25)) => (list 5 0))
(check (list (exact-integer-sqrt 100)) => (list 10 0))
(check (list (exact-integer-sqrt 1000)) => (list 31 39))
(check (list (exact-integer-sqrt 1000000)) => (list 1000 0))
(check-catch 'type-error (exact-integer-sqrt "a"))
(check-catch 'value-error (exact-integer-sqrt -1))
(check-catch 'type-error (exact-integer-sqrt 1.1))
(check-catch 'type-error (exact-integer-sqrt 1+i)) 

(check-report)
