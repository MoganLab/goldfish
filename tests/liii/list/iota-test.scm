(import (liii list) (liii check))


(check-set-mode! 'report-failed)


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
;; ----
;; list?
;; 返回一个由连续整数组成的列表。
;;
;; 说明
;; ----
;; iota函数按照SRFI-1规范实现，用于生成等差数列。
;; - 当只提供一个参数count时，生成从0开始的连续整数序列。
;; - 当提供count和start参数时，生成从start开始的连续整数序列。
;; - 当提供count、start和step参数时，生成从start开始，步长为step的整数序列。
;;
;; 错误处理
;; ----
;; value-error 当count为负数时抛出。
;; type-error 当任何参数不是整数时抛出。


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
(check (iota 4 10 -2)
  =>
  (list 10 8 6 4)
) ;check


(check (iota 3 7 0) => (list 7 7 7))


(check (iota 3 -5) => (list -5 -4 -3))
(check (iota 4 -10 2)
  =>
  (list -10 -8 -6 -4)
) ;check


(check-catch 'value-error (iota -1))
(check-catch 'value-error (iota -5))
(check-catch 'type-error (iota 'a))
(check-catch 'type-error (iota 3 'a))
(check-catch 'type-error (iota 3 5 'a))
(check-catch 'type-error (iota 3.5))
(check-catch 'type-error (iota 3 5.5))
(check-catch 'type-error (iota 3 2 0.5))


(check-report)
