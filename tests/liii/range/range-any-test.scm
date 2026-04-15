(import (liii check) (liii range))


(check-set-mode! 'report-failed)


;; range-any
;; 检查是否存在满足谓词的元素。
;;
;; 语法
;; ----
;; (range-any pred r)
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
;; 如果存在满足谓词的元素，返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (range-any even? (numeric-range 0 10)) => #t
;; (range-any (lambda (x) (> x 8)) (numeric-range 0 10)) => #t
;; (range-any (lambda (x) (> x 10)) (numeric-range 0 10)) => #f
;;
;; 错误处理
;; ----
;; 无


(let ((r (numeric-range 0 10)))
  (check-true (range-any even? r))
  (check-true (range-any (lambda (x) (> x 8)) r)
  ) ;check-true
  (check-false (range-any (lambda (x) (> x 10)) r)
  ) ;check-false
  (check-true (range-any (lambda (x) (= x 0)) r)
  ) ;check-true
  (check-false (range-any (lambda (x) (< x 0)) r)
  ) ;check-false
) ;let


(let ((r (numeric-range 1 10)))
  (check-true (range-any odd? r))
  (check-false (range-any (lambda (x) (= x 0)) r)
  ) ;check-false
) ;let


(check-report)
