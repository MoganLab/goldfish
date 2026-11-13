;; decimal-negative?
;; 判断 decimal 是否为负数。
;;
;; 语法
;; ----
;; (decimal-negative? n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要判断的 decimal。
;;
;; 返回值
;; ----
;; boolean?
;;   负数返回 #t，否则返回 #f。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(define (d str)
  (string->decimal str)
) ;define

(check-true (decimal-negative? (d "-0.01")))
(check-false (decimal-negative? (d "0")))
(check-false (decimal-negative? (d "0.01")))

(check-catch 'type-error (decimal-negative? -1))

(check-report)
