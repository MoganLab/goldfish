(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range-remove->list
;; 过滤 range 中不满足谓词的元素，结果为列表。
;;
;; 语法
;; ----
;; (range-remove->list pred r)
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
;; 列表，包含所有不满足谓词的元素。
;;
;; 示例
;; ----
;; (range-remove->list even? (numeric-range 0 10)) => '(1 3 5 7 9)
;; (range-remove->list odd? (numeric-range 0 10)) => '(0 2 4 6 8)
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 10)))
  (check (range-remove->list even? r) => '(1 3 5 7 9))
  (check (range-remove->list odd? r) => '(0 2 4 6 8))
  (check (range-remove->list (lambda (x) (> x 5)) r) => '(0 1 2 3 4 5))
  (check (range-remove->list (lambda (x) (< x 5)) r) => '(5 6 7 8 9))
) ;let

(let ((r (numeric-range 0 0)))
  (check (range-remove->list even? r) => '())
) ;let

(let ((r (numeric-range 0 5)))
  (check (range-remove->list (lambda (x) #t) r) => '())
  (check (range-remove->list (lambda (x) #f) r) => '(0 1 2 3 4))
) ;let

(check-report)
