(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

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
;; 示例
;; ----
;; (range-map->vector (lambda (x) (* x 2)) (numeric-range 0 5)) => #(0 2 4 6 8)
;;
;; 错误处理
;; ----
;; 无

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

(check-report)
