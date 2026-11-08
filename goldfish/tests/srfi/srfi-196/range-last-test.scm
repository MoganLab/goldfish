;; range-last
;; 获取 range 的最后一个元素。
;;
;; 语法
;; ----
;; (range-last r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; any
;; 最后一个元素。
;;
;; 注意
;; ----
;; 等价于 (range-ref r (- (range-length r) 1))。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range 时抛出。
;; - value-error：当 r 为空 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(let ((r (numeric-range 10 20)))
  (check (range-last r) => 19)
) ;let


(let ((r (numeric-range 0 10)))
  (check (range-last r) => 9)
) ;let


(let ((r (numeric-range 5 0 -1)))
  (check (range-last r) => 1)
) ;let


(let ((r (vector-range #(a b c d e))))
  (check (range-last r) => 'e)
) ;let

;; r 为空 range 时抛出 value-error
(check-catch 'value-error (range-last (numeric-range 0 0)))
(check-catch 'value-error (range-last (range 0 (lambda (i) i))))

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-last 42))
(check-catch 'type-error (range-last "not-a-range"))
(check-catch 'type-error (range-last '(0 1 2)))


(check-report)
