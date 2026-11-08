;; truncate-remainder
;; 截断除法：返回余数，符号与被除数相同。
;;
;;
;; 语法
;; ----
;; (truncate-remainder x y)
;;
;; 参数
;; ----
;; x : real?
;; 被除数。
;;
;; y : real?
;; 除数，不能为零。
;;
;; 返回值
;; ----
;; number?
;; 余数 r 满足 x = q*y + r，其中 q 为 (truncate-quotient x y)；
;; r 的符号与 x 相同（或为零）。
;;
;; 说明
;; ----
;; R7RS 6.2.6：(truncate-remainder x y) 等价于 R5RS 的 remainder。
;; 与 floor-remainder（即 modulo）的区别：
;;   (truncate-remainder -11 3) => -2（符号随被除数）
;;   (floor-remainder -11 3)    =>  1（符号随除数）
;;
;; 错误处理
;; ----
;; type-error
;; 当参数不是实数时抛出错误。
;; division-by-zero
;; 当除数为零时抛出错误。

(import (liii check))
(import (liii base))
(import (scheme base))

(check-set-mode! 'report-failed)

;; 四种符号组合（余数符号随被除数）
(check (truncate-remainder 11 3) => 2)
(check (truncate-remainder 11 -3) => 2)
(check (truncate-remainder -11 3) => -2)
(check (truncate-remainder -11 -3) => -2)

;; 整除与零
(check (truncate-remainder 10 2) => 0)
(check (truncate-remainder 10 -2) => 0)
(check (truncate-remainder -10 2) => 0)
(check (truncate-remainder -10 -2) => 0)
(check (truncate-remainder 0 5) => 0)
(check (truncate-remainder 1 3) => 1)
(check (truncate-remainder -1 3) => -1)

;; 与 truncate/ 及 remainder 的一致性
(check (truncate-remainder 15 4) => (receive (q r) (truncate/ 15 4) r))
(check (truncate-remainder -15 4) => (receive (q r) (truncate/ -15 4) r))
(check (truncate-remainder -11 3) => (remainder -11 3))

;; 错误场景
(check-catch 'division-by-zero (truncate-remainder 11 0))
(check-catch 'division-by-zero (truncate-remainder 0 0))
(check-catch 'type-error (truncate-remainder 1.0+1.0i 2))
(check-catch 'type-error (truncate-remainder 11 "3"))

(check-report)
