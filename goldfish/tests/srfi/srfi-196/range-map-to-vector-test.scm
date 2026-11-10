;; range-map->vector
;; 将映射函数应用于 range 的每个元素，结果收集为向量。
;;
;; 语法
;; ----
;; (range-map->vector proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; vector
;; 向量，包含映射后的结果。
;;
;; 错误处理
;; ----
;; - type-error：当 proc 不是过程，或 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 5)))
  (check (range-map->vector (lambda (x) (* x 2)) r) => #(0 2 4 6 8))
) ;let


(let ((r (numeric-range 0 5)))
  (check (range-map->vector (lambda (x) (* x x)) r) => #(0 1 4 9 16))
) ;let


(let ((r (numeric-range 0 0)))
  (check (range-map->vector (lambda (x) (* x 2)) r) => #())
) ;let


(let ((r (numeric-range 1 6)))
  (check (range-map->vector (lambda (x) (+ x 10)) r) => #(11 12 13 14 15))
) ;let



;; proc 不是过程时抛出 type-error
(check-catch 'type-error (range-map->vector 42 (numeric-range 0 5)))
(check-catch 'type-error (range-map->vector "not-a-proc" (numeric-range 0 5)))
(check-catch 'type-error (range-map->vector 'pred (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-map->vector (lambda (x) x) 42))
(check-catch 'type-error (range-map->vector (lambda (x) x) "not-a-range"))

(check-report)
