;; range->vector
;; 将 range 转换为向量。
;;
;; 语法
;; ----
;; (range->vector r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; vector
;; 包含 range 所有元素的向量。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 5)))
  (check (range->vector r) => #(0 1 2 3 4))
) ;let


(let ((r (numeric-range 0 0)))
  (check (range->vector r) => #())
) ;let


(let ((r (numeric-range 10 20 2)))
  (check (range->vector r) => #(10 12 14 16 18))
) ;let


(let ((r (vector-range #(a b c d e))))
  (check (range->vector r) => #(a b c d e))
) ;let



;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range->vector 42))
(check-catch 'type-error (range->vector "not-a-range"))
(check-catch 'type-error (range->vector '(0 1 2)))

(check-report)
