;; max
;; 返回所有给定实数的最大值。
;;
;;
;; 语法
;; ----
;; (max num ...)
;;
;; 参数
;; ----
;; num : real?
;; 任意个实数（大于等于1）。
;;
;; 返回值
;; ----
;; real?
;; 返回所给所有值的最大值。
;; 如果存在NaN，返回NaN。
;; 如果参数中存在不精确值，返回值也是不精确的，否则返回值是精确的
;;
;; 说明
;; ----
;; 与 S7 原生函数 `s7-max` 的区别：
;; - `max` 遵循 R7RS：只要有一个参数不精确，结果就不精确（(max 1 2.0) => 2.0）；
;;   `s7-max` 直接返回被选中的原始参数，不额外做精确性转换。
;; - `max` 逐个校验参数必须是实数并统一抛出 type-error，`s7-max` 交由 S7 内建报错。
;;
;; 选择建议：常规代码用 `max`（R7RS 语义、错误类型统一、可移植）；
;; 需要直接拿到被选中的原始参数或追求 S7 原生性能时用 `s7-max`。
;;
;; 参见: gf doc liii/s7 "s7-max"
;;
;; 错误处理
;; ----
;; type-error
;; 如果存在任何参数不是实数，抛出错误。
;; wrong-number-of-args
;; 如果没有提供参数，抛出错误。

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(check (max 7) => 7)
(check (max 3.5) => 3.5)
(check (max 1/3) => 1/3)
(check (max +inf.0) => +inf.0)
(check (max -inf.0) => -inf.0)
(check (nan? (max +nan.0)) => #t)
(check (max 7 3) => 7)
(check (max 3.0 7.0) => 7.0)
(check (max 3 7.0) => 7.0)
(check (max 7.0 3) => 7.0)
(check (max 1/2 1/3) => 1/2)
(check (max 1/3 2/3) => 2/3)
(check (max +inf.0 7) => +inf.0)
(check (max 7 +inf.0) => +inf.0)
(check (max -inf.0 7) => 7.0)
(check (max 7 -inf.0) => 7.0)
(check (nan? (max +nan.0 7)) => #t)
(check (nan? (max 7 +nan.0)) => #t)
(check (max 7 3 5) => 7)
(check (max 3.0 7.0 2.0) => 7.0)
(check (max 7 3.0 5) => 7.0)
(check (max 1/2 1/3 2/3) => 2/3)
(check (max +inf.0 7 3) => +inf.0)
(check (max -inf.0 7 3) => 7.0)
(check (nan? (max +nan.0 7 3)) => #t)
(check (nan? (max 7 +nan.0 3)) => #t)
(check (nan? (max +nan.0 +inf.0 -inf.0)) => #t)
(check (max 7 3.0 5/4) => 7.0)
(check (max 5.0 7/2 8) => 8.0)
(check (max +inf.0 7 3/4) => +inf.0)
(check (max -inf.0 7 3.0) => 7.0)
(check (nan? (max +nan.0 7.0 3)) => #t)
(check (nan? (max 7/3 +nan.0 3)) => #t)
(check-catch 'wrong-number-of-args (max))
(check-catch 'type-error (max 'hello 7))
(check-catch 'type-error (max "world" 7))
(check-catch 'type-error (max #t 7))
(check-catch 'type-error (max #f 7))
(check-catch 'type-error (max '(1 3 5) 7))
(check-catch 'type-error (max '() 7))
(check-catch 'type-error (max 1.0+2.0i 2))

(check-report)
