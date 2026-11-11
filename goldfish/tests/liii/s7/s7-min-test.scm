;; s7-min
;; S7 内置的求最小值函数，直接返回被选中的原始参数。
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
;; 1. 与 `min` 的区别：`s7-min` 直接返回被选中的原始参数（(s7-min 1 2.0) => 1），
;;    而 `min` 只要有一个参数不精确，结果就不精确（(min 1 2.0) => 1.0）。
;; 2. `s7-min` 不做 R7RS 的参数检查，非实数参数会抛出 S7 原生错误。
;; 3. 至少需要一个参数。
;;
;; 选择建议：常规代码用 `min`；需要直接拿到被选中的原始参数或追求 S7 原生性能时用 `s7-min`。
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
