;; iota
;; 生成一个等差数列列表。
;;
;; 语法
;; ----
;; (iota count)
;; (iota count start)
;; (iota count start step)
;;
;; 参数
;; ----
;; count : exact-nonnegative-integer?
;; 需要生成的元素个数，必须是非负整数。
;; start : integer?
;; 数列的起始值，默认为0。
;; step : integer?
;; 数列的步长，默认为1。
;;
;; 返回值
;; ------
;; list?
;; 返回一个由连续整数组成的列表。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (iota 3) => (list 0 1 2))
(check (iota 3 7) => (list 7 8 9))
(check (iota 2 7 2) => (list 7 9))

(check (iota 0) => '())
(check (iota 1) => '(0))
(check (iota 1 5) => '(5))
(check (iota 1 5 2) => '(5))

(check (iota 5) => (list 0 1 2 3 4))
(check (iota 5 1 2) => (list 1 3 5 7 9))
(check (iota 3 0 -1) => (list 0 -1 -2))
(check (iota 4 10 -2) => (list 10 8 6 4))

(check (iota 3 7 0) => (list 7 7 7))

(check (iota 3 -5) => (list -5 -4 -3))
(check (iota 4 -10 2) => (list -10 -8 -6 -4))

(check-catch 'value-error (iota -1))
(check-catch 'value-error (iota -5))
(check-catch 'type-error (iota 'a))
(check-catch 'type-error (iota 3 'a))
(check-catch 'type-error (iota 3 5 'a))
(check-catch 'type-error (iota 3.5))
(check-catch 'type-error (iota 3 5.5))
(check-catch 'type-error (iota 3 2 0.5))
(check-catch 'value-error (iota 2 9223372036854775807 1))
(check-catch 'value-error (iota 2 -9223372036854775808 -1))
(check-catch 'value-error (iota 3 4611686018427387904 4611686018427387904))
(check-catch 'value-error (iota 9223372036854775807 0 2))

;; 极值贴边但合法的边界测试（防止末次迭代多算一次递减产生溢出 UB）
(check (iota 2 9223372036854775807 -1) => '(9223372036854775807 9223372036854775806))
(check (iota 2 -9223372036854775808 1) => '(-9223372036854775808 -9223372036854775807))
(check (iota 1 9223372036854775807 -1) => '(9223372036854775807))
(check (iota 1 -9223372036854775808 1) => '(-9223372036854775808))

(check-report)
