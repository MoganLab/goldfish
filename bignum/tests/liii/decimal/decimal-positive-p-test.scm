;; decimal-positive?
;; 判断 decimal 是否为正数。
;;
;; 语法
;; ----
;; (decimal-positive? n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要判断的 decimal。
;;
;; 返回值
;; ----
;; boolean?
;;   正数返回 #t，否则返回 #f。
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

(check-true (decimal-positive? (d "0.01")))
(check-false (decimal-positive? (d "0")))
(check-false (decimal-positive? (d "-0.01")))

(check-catch 'type-error (decimal-positive? 1))

(check-report)
