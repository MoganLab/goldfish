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
;; 错误处理
;; ----
;; - type-error：当 pred 不是过程，或 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




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



;; pred 不是过程时抛出 type-error
(check-catch 'type-error (range-count 42 (numeric-range 0 5)))
(check-catch 'type-error (range-count "not-a-proc" (numeric-range 0 5)))
(check-catch 'type-error (range-count 'pred (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-count (lambda (x) x) 42))
(check-catch 'type-error (range-count (lambda (x) x) "not-a-range"))

(check-report)
