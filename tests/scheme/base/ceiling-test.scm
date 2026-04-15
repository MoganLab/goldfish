(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; ceiling
;; 返回不小于给定数的最小整数。
;;
;; 语法
;; ----
;; (ceiling num )
;;
;; 参数
;; ----
;; num : real?
;; 实数
;;
;; 返回值
;; ------
;; 返回不小于给定数的最小整数
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 错误
;; ----
;; wrong-type-arg
;; 如果参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果参数数量不为一，抛出错误。
(check (ceiling 1.1) => 2.0)
(check (ceiling 1) => 1)
(check (ceiling 1/2) => 1)
(check (ceiling 0) => 0)
(check (ceiling -1) => -1)
(check (ceiling -1.2) => -1.0)
(check-catch 'wrong-type-arg (ceiling 2.0+4.0i))
(check-catch 'wrong-type-arg (ceiling 'hello'))
(check-catch 'wrong-number-of-args (ceiling 4 5))
(check (s7-ceiling 1.1) => 2)
(check (s7-ceiling -1.2) => -1)
(check-report)