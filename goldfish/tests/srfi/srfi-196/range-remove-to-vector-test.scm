;; range-remove->vector
;; 过滤 range 中不满足谓词的元素，结果为向量。
;;
;; 语法
;; ----
;; (range-remove->vector pred r)
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
;; vector
;; 向量，包含所有不满足谓词的元素。
;;
;; 错误处理
;; ----
;; - type-error：当 pred 不是过程，或 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 10)))
  (check (range-remove->vector even? r) => #(1 3 5 7 9))
) ;let


(let ((r (numeric-range 0 10)))
  (check (range-remove->vector odd? r) => #(0 2 4 6 8))
) ;let


(let ((r (numeric-range 0 0)))
  (check (range-remove->vector even? r) => #())
) ;let


(let ((r (numeric-range 0 5)))
  (check (range-remove->vector (lambda (x) (> x 2)) r) => #(0 1 2))
) ;let



;; pred 不是过程时抛出 type-error
(check-catch 'type-error (range-remove->vector 42 (numeric-range 0 5)))
(check-catch 'type-error
  (range-remove->vector "not-a-proc" (numeric-range 0 5))
) ;check-catch
(check-catch 'type-error (range-remove->vector 'pred (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-remove->vector (lambda (x) x) 42))
(check-catch 'type-error (range-remove->vector (lambda (x) x) "not-a-range"))

(check-report)
