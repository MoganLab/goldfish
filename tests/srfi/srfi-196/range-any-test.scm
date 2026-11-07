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
;; 错误处理
;; ----
;; - type-error：当 pred 不是过程，或 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 10)))
  (check-true (range-any even? r))
  (check-true (range-any (lambda (x) (> x 8)) r))
  (check-false (range-any (lambda (x) (> x 10)) r))
  (check-true (range-any (lambda (x) (= x 0)) r))
  (check-false (range-any (lambda (x) (< x 0)) r))
) ;let


(let ((r (numeric-range 1 10)))
  (check-true (range-any odd? r))
  (check-false (range-any (lambda (x) (= x 0)) r))
) ;let



;; pred 不是过程时抛出 type-error
(check-catch 'type-error (range-any 42 (numeric-range 0 5)))
(check-catch 'type-error (range-any "not-a-proc" (numeric-range 0 5)))
(check-catch 'type-error (range-any 'pred (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-any (lambda (x) x) 42))
(check-catch 'type-error (range-any (lambda (x) x) "not-a-range"))

(check-report)
