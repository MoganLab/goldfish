;; truncate/
;; 执行截断除法，返回商和余数。
;;
;;
;; 语法
;; ----
;; (truncate/ dividend divisor)
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
;; ----
;; 返回两个值：
;; 1. 向靠近零方向取整的商
;; 2. 余数，满足 dividend = quotient * divisor + remainder
;;
;; 错误处理
;; ----
;; division-by-zero
;; 当除数为 0 时抛出错误。
;; type-error
;; 当参数不是实数时抛出错误。

(import (liii check))
(import (liii base))
(import (scheme base))

(check-set-mode! 'report-failed)

;; truncate/ 基本测试
(check (receive (q r) (truncate/ 11 3) q) => 3)
(check (receive (q r) (truncate/ 11 3) r) => 2)
(check (receive (q r) (truncate/ 11 -3) q) => -3)
(check (receive (q r) (truncate/ 11 -3) r) => 2)
(check (receive (q r) (truncate/ -11 3) q) => -3)
(check (receive (q r) (truncate/ -11 3) r) => -2)
(check (receive (q r) (truncate/ -11 -3) q) => 3)
(check (receive (q r) (truncate/ -11 -3) r) => -2)
(check (receive (q r) (truncate/ 10 2) q) => 5)
(check (receive (q r) (truncate/ 10 2) r) => 0)
(check (receive (q r) (truncate/ 10 -2) q) => -5)
(check (receive (q r) (truncate/ 10 -2) r) => 0)
(check (receive (q r) (truncate/ -10 2) q) => -5)
(check (receive (q r) (truncate/ -10 2) r) => 0)
(check (receive (q r) (truncate/ -10 -2) q) => 5)
(check (receive (q r) (truncate/ -10 -2) r) => 0)
(check (receive (q r) (truncate/ 15 4) q) => 3)
(check (receive (q r) (truncate/ 15 4) r) => 3)
(check (receive (q r) (truncate/ 15 -4) q) => -3)
(check (receive (q r) (truncate/ 15 -4) r) => 3)
(check (receive (q r) (truncate/ -15 4) q) => -3)
(check (receive (q r) (truncate/ -15 4) r) => -3)
(check (receive (q r) (truncate/ -15 -4) q) => 3)
(check (receive (q r) (truncate/ -15 -4) r) => -3)
(check (receive (q r) (truncate/ 1 3) q) => 0)
(check (receive (q r) (truncate/ 1 3) r) => 1)
(check (receive (q r) (truncate/ 0 5) q) => 0)
(check (receive (q r) (truncate/ 0 5) r) => 0)
(check-catch 'division-by-zero (truncate/ 11 0))
(check-catch 'division-by-zero (truncate/ 0 0))
(check-catch 'type-error (truncate/ 1.0+1.0i 2))
(check-catch 'type-error (truncate/ 5 #t))

(check-report)
