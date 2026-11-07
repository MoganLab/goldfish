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
;; 错误处理
;; ----
;; - type-error：当 pred 不是过程，或 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




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



;; pred 不是过程时抛出 type-error
(check-catch 'type-error (range-every 42 (numeric-range 0 5)))
(check-catch 'type-error (range-every "not-a-proc" (numeric-range 0 5)))
(check-catch 'type-error (range-every 'pred (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-every (lambda (x) x) 42))
(check-catch 'type-error (range-every (lambda (x) x) "not-a-range"))

(check-report)
