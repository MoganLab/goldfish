(import (liii check)
        (srfi srfi-196)
) ;import

(check-set-mode! 'report-failed)

;; range-count
;; 统计满足谓词的元素数量。
;;
;; 语法
;; ----
;; (range-count pred r)
;;
;; 参数
;; ----
;; pred : procedure
;; 谓词函数，接受一个元素返回布尔值。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; exact-natural
;; 满足谓词的元素数量。
;;
;; 示例
;; ----
;; (range-count even? (numeric-range 0 10)) => 5
;; (range-count odd? (numeric-range 0 10)) => 5
;;
;; 错误处理
;; ----
;; 无

(let ((r (numeric-range 0 10)))
  (check (range-count even? r) => 5)
  (check (range-count odd? r) => 5)
  (check (range-count (lambda (x) (> x 5)) r) => 4)
  (check (range-count (lambda (x) (< x 5)) r) => 5)
  (check (range-count (lambda (x) (= x 0)) r) => 1)
  (check (range-count (lambda (x) (< x 0)) r) => 0)
) ;let

(let ((r (numeric-range 0 5)))
  (check (range-count (lambda (x) #t) r) => 5)
  (check (range-count (lambda (x) #f) r) => 0)
) ;let

(check-report)
