(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range-map->list
;; 将映射函数应用于 range 的每个元素，结果收集为列表。
;;
;; 语法
;; ----
;; (range-map->list proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 映射函数，接受一个元素。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; list
;; 列表，包含映射后的结果。
;;
;; 示例
;; ----
;; (range-map->list (lambda (x) (* x 2)) (numeric-range 0 5)) => '(0 2 4 6 8)
;; (range-map->list (lambda (x) (* x x)) (numeric-range 0 5)) => '(0 1 4 9 16)
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 5)))
  (check (range-map->list (lambda (x) (* x 2)) r) => '(0 2 4 6 8))
  (check (range-map->list (lambda (x) (* x x)) r) => '(0 1 4 9 16))
  (check (range-map->list (lambda (x) (+ x 10)) r) => '(10 11 12 13 14))
) ;let

(let ((r (numeric-range 1 6)))
  (check (range-map->list (lambda (x) (* x x)) r) => '(1 4 9 16 25))
) ;let

(let ((r (numeric-range 0 0)))
  (check (range-map->list (lambda (x) (* x 2)) r) => '())
) ;let

(check-report)
