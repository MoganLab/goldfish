;; range-length
;; 获取 range 的长度。
;;
;; 语法
;; ----
;; (range-length r)
;;
;; 参数
;; ----
;; r : range
;; range 对象。
;;
;; 返回值
;; ----
;; exact-natural
;; range 的长度，非负整数。
;;
;; 错误处理
;; ----
;; - type-error：当 r 不是 range 时抛出。

(import (liii check) (srfi srfi-196))


(check-set-mode! 'report-failed)




(check (range-length (numeric-range 0 10)) => 10)
(check (range-length (numeric-range 0 5)) => 5)
(check (range-length (numeric-range 0 0)) => 0)
(check (range-length (range 5 (lambda (i) i))) => 5)
(check (range-length (range 0 (lambda (i) i))) => 0)


(let ((r (numeric-range 10 30 2)))
  (check (range-length r) => 10)
) ;let


(let ((r (numeric-range 5 0 -1)))
  (check (range-length r) => 5)
) ;let

;; r 不是 range 时抛出 type-error
(check-catch 'type-error (range-length 42))
(check-catch 'type-error (range-length "not-a-range"))
(check-catch 'type-error (range-length '(0 1 2)))


(check-report)
