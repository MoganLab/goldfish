;; s7-max
;; S7 内置的求最大值函数，直接返回被选中的原始参数。
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
;; 1. 与 `max` 的区别：`s7-max` 直接返回被选中的原始参数（(s7-max 1 2.0) => 2.0），
;;    而 `max` 只要有一个参数不精确，结果就不精确（(max 1 2.0) => 2.0）。
;; 2. `s7-max` 不做 R7RS 的参数检查，非实数参数会抛出 S7 原生错误。
;; 3. 至少需要一个参数。
;;
;; 选择建议：常规代码用 `max`；需要直接拿到被选中的原始参数或追求 S7 原生性能时用 `s7-max`。
;;
;; 参见: gf doc scheme/base "max"

(import (liii check) (liii s7))

(check-set-mode! 'report-failed)

(check (s7-max 7) => 7)
(check (s7-max 3 1 4) => 4)
(check (s7-max 5 2) => 5)
(check (s7-max -3 -10 0) => 0)

(check-catch 'wrong-number-of-args (s7-max))

(check-report)
