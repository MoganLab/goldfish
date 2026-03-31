(import (liii check)
        (liii range)
) ;import

(check-set-mode! 'report-failed)

;; range-filter->list
;; 过滤 range 中满足谓词的元素，结果为列表。
;;
;; 语法
;; ----
;; (range-filter->list pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; list
;; 列表，包含所有满足谓词的元素。
;;
;; 示例
;; ----
;; (range-filter->list even? (numeric-range 0 10)) => '(0 2 4 6 8)
;; (range-filter->list odd? (numeric-range 0 10)) => '(1 3 5 7 9)
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 10)))
  (check (range-filter->list even? r) => '(0 2 4 6 8))
  (check (range-filter->list odd? r) => '(1 3 5 7 9))
  (check (range-filter->list (lambda (x) (> x 5)) r) => '(6 7 8 9))
  (check (range-filter->list (lambda (x) (< x 5)) r) => '(0 1 2 3 4))
) ;let

(let ((r (numeric-range 0 0)))
  (check (range-filter->list even? r) => '())
) ;let

(let ((r (numeric-range 0 5)))
  (check (range-filter->list (lambda (x) #t) r) => '(0 1 2 3 4))
  (check (range-filter->list (lambda (x) #f) r) => '())
) ;let

(check-report)
