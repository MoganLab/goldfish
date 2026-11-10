;; range-first
;; 获取 range 的第一个元素。
;;
;; 语法
;; ----
;; (range-first r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; any
;; 第一个元素。
;;
;; 注意
;; ----
;; 等价于 (range-ref r 0)。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range 时抛出。
;; - value-error：当 r 为空 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 10 20)))
  (check (range-first r) => 10)
) ;let


(let ((r (numeric-range 0 10)))
  (check (range-first r) => 0)
) ;let


(let ((r (vector-range #(a b c d e))))
  (check (range-first r) => 'a)
) ;let

;; r 为空 range 时抛出 value-error
(check-catch 'value-error (range-first (numeric-range 0 0)))
(check-catch 'value-error (range-first (range 0 (lambda (i) i))))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-first 42))
(check-catch 'type-error (range-first "not-a-range"))
(check-catch 'type-error (range-first '(0 1 2)))


(check-report)
