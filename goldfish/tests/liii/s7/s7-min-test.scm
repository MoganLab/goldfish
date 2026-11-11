;; s7-min
;; S7 内置的求最小值函数。
;;
;; 语法
;; ----
;; (s7-min num ...)
;;
;; 参数
;; ----
;; num : real?
;; 一个或多个实数。
;;
;; 返回值
;; ----
;; real?
;; 参数中的最小值。
;;
;; 说明
;; ----
;; 1. 与 min 基本行为一致
;; 2. 至少需要一个参数
;;
;; 参见: gf doc scheme/base "min"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-min 7) => 7)
(check (s7-min 3 1 4) => 1)
(check (s7-min 5 2) => 2)
(check (s7-min -3 -10 0) => -10)

(check-catch 'wrong-number-of-args (s7-min))

(check-report)
