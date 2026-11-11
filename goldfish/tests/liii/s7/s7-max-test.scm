;; s7-max
;; S7 内置的求最大值函数。
;;
;; 语法
;; ----
;; (s7-max num ...)
;;
;; 参数
;; ----
;; num : real?
;; 一个或多个实数。
;;
;; 返回值
;; ----
;; real?
;; 参数中的最大值。
;;
;; 说明
;; ----
;; 1. 与 max 基本行为一致
;; 2. 至少需要一个参数

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-max 7) => 7)
(check (s7-max 3 1 4) => 4)
(check (s7-max 5 2) => 5)
(check (s7-max -3 -10 0) => 0)

(check-catch 'wrong-number-of-args (s7-max))

(check-report)
