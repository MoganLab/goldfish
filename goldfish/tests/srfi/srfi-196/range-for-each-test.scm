;; range-for-each
;; 对 range 的每个元素执行副作用操作。
;;
;; 语法
;; ----
;; (range-for-each proc r)
;;
;; 参数
;; ----
;; proc : procedure
;; 副作用函数，接受一个元素。
;;
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; undefined
;; 无（未定义）。
;;
;; 错误处理
;; ----
;; - type-error：当 proc 不是过程，或 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 0 5)) (result '()))
  (range-for-each (lambda (x) (set! result (cons x result))) r)
  (check result => '(4 3 2 1 0))
) ;let


(let ((r (numeric-range 0 0)) (result '()))
  (range-for-each (lambda (x) (set! result (cons x result))) r)
  (check result => '())
) ;let


(let ((r (numeric-range 1 4)) (sum 0))
  (range-for-each (lambda (x) (set! sum (+ sum x))) r)
  (check sum => 6)
) ;let



;; proc 不是过程时抛出 type-error
(check-catch 'type-error (range-for-each 42 (numeric-range 0 5)))
(check-catch 'type-error (range-for-each "not-a-proc" (numeric-range 0 5)))
(check-catch 'type-error (range-for-each 'pred (numeric-range 0 5)))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-for-each (lambda (x) x) 42))
(check-catch 'type-error (range-for-each (lambda (x) x) "not-a-range"))

(check-report)
