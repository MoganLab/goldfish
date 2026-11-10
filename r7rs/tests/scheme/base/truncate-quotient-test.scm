;; truncate-quotient
;; 截断除法：返回向零取整的商。
;;
;;
;; 语法
;; ----
;; (truncate-quotient x y)
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
;; 向零方向取整的商，满足 x/y 向零取整。
;;
;; 说明
;; ----
;; R7RS 6.2.6：(truncate-quotient x y) 等价于 (truncate (/ x y))。
;; 与 floor-quotient 的区别：负数除法的商向零取整而非向负无穷。
;;   (truncate-quotient -11 2) => -5
;;   (floor-quotient -11 2)    => -6
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

;; 四种符号组合（商向零取整）
(check (truncate-quotient 11 2) => 5)
(check (truncate-quotient 11 -2) => -5)
(check (truncate-quotient -11 2) => -5)
(check (truncate-quotient -11 -2) => 5)

;; 整除与零
(check (truncate-quotient 10 2) => 5)
(check (truncate-quotient 10 -2) => -5)
(check (truncate-quotient -10 2) => -5)
(check (truncate-quotient -10 -2) => 5)
(check (truncate-quotient 0 5) => 0)
(check (truncate-quotient 0 -5) => 0)
(check (truncate-quotient 1 3) => 0)
(check (truncate-quotient -1 3) => 0)

;; 与 truncate/ 一致性
(check (truncate-quotient 15 4) => (receive (q r) (truncate/ 15 4) q))
(check (truncate-quotient -15 4) => (receive (q r) (truncate/ -15 4) q))

;; 错误场景
(check-catch 'division-by-zero (truncate-quotient 11 0))
(check-catch 'division-by-zero (truncate-quotient 0 0))
(check-catch 'type-error (truncate-quotient 1.0+1.0i 2))
(check-catch 'type-error (truncate-quotient "11" 2))

(check-report)
