(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; floor/
;; 执行地板除法，返回商和余数。
;;
;; 语法
;; ----
;; (floor/ dividend divisor)
;;
;; 参数
;; ----
;; dividend : real?
;; 被除数。
;;
;; divisor : real?
;; 除数，不能为零。
;;
;; 返回值
;; ------
;; 返回两个值：
;; 1. 向下取整的商
;; 2. 余数

;; 基本测试
(check (list (floor/ 17 5)) => (list 3 2))
(check (list (floor/ 10 3)) => (list 3 1))
(check (list (floor/ 20 4)) => (list 5 0))

;; 负数测试
(check (list (floor/ -17 5)) => (list -4 3))
(check (list (floor/ 17 -5)) => (list -4 -3))
(check (list (floor/ -17 -5)) => (list 3 -2))

;; 小数测试
(check (list (floor/ 10.0 3)) => (list 3.0 1.0))
(check (list (floor/ 10 3.0)) => (list 3.0 1.0))

;; 边界测试
(check (list (floor/ 0 5)) => (list 0 0))
(check (list (floor/ 5 5)) => (list 1 0))
(check (list (floor/ 1 5)) => (list 0 1))

;; 错误测试
(check-catch 'division-by-zero (floor/ 10 0))
(check-catch 'wrong-type-arg (floor/ "10" 5))
(check-catch 'wrong-type-arg (floor/ 10 "5"))

(check-report)
