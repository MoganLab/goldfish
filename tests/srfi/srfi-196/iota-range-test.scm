;; iota-range
;; 创建 iota 序列的 range（类似于 iota 函数）。
;;
;; 语法
;; ----
;; (iota-range len)
;; (iota-range len start)
;; (iota-range len start step)
;;
;; 参数
;; ----
;; len : exact-natural
;; 序列长度。
;;
;; start : real (可选)
;; 起始值，默认为 0。
;;
;; step : real (可选)
;; 步长，默认为 1。
;;
;; 返回值
;; ----
;; range
;; 包含 iota 序列的 range 对象。
;;
;; 注意
;; ----
;; 创建等差数列，类似于 (start, start+step, start+2*step, ...)。
;;
;; 错误处理
;; ----
;; - type-error：当 len 不是精确整数，或 start、step 不是实数时抛出。
;; - value-error：当 len 是负整数时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (iota-range 10)))
  (check (range-length r) => 10)
  (check (range-ref r 0) => 0)
  (check (range-ref r 9) => 9)
) ;let


(let ((r (iota-range 5 10)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 10)
  (check (range-ref r 4) => 14)
) ;let


(let ((r (iota-range 5 0 2)))
  (check (range-length r) => 5)
  (check (range-ref r 0) => 0)
  (check (range-ref r 1) => 2)
  (check (range-ref r 4) => 8)
) ;let

;; len 为 0 的空 range 合法
(check (range-length (iota-range 0)) => 0)

;; len 不是精确整数时抛出 type-error
(check-catch 'type-error (iota-range 'a))
(check-catch 'type-error (iota-range "10"))
(check-catch 'type-error (iota-range 10.5))
(check-catch 'type-error (iota-range 10 'b))
(check-catch 'type-error (iota-range 10 0 'c))

;; len 为负数时抛出 value-error
(check-catch 'value-error (iota-range -1))
(check-catch 'value-error (iota-range -5 10 2))

;; start 或 step 不是实数时抛出 type-error
(check-catch 'type-error (iota-range 5 'start))
(check-catch 'type-error (iota-range 5 0 "2"))


(check-report)
