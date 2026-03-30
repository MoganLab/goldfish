(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range-every
;; 检查是否所有元素都满足谓词。
;;
;; 语法
;; ----
;; (range-every pred r)
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
;; boolean
;; 如果所有元素都满足谓词，返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (range-every integer? (numeric-range 0 10)) => #t
;; (range-every even? (numeric-range 0 10)) => #f
;; (range-every (lambda (x) (< x 10)) (numeric-range 0 10)) => #t
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 10)))
  (check-true (range-every integer? r))
  (check-false (range-every even? r))
  (check-true (range-every (lambda (x) (< x 10)) r))
  (check-false (range-every (lambda (x) (> x 5)) r))
) ;let

(let ((r (numeric-range 0 10 2)))
  (check-true (range-every even? r))
  (check-false (range-every odd? r))
) ;let

(let ((r (numeric-range 0 0)))
  (check-true (range-every (lambda (x) #f) r))
) ;let

(check-report)
