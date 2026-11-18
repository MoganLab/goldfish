;; decimal-zero?
;; 判断 decimal 是否为零。
;;
;; 语法
;; ----
;; (decimal-zero? n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要判断的 decimal。
;;
;; 返回值
;; ----
;; boolean?
;;   零返回 #t，否则返回 #f。
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

(check-true (decimal-zero? (d "0.00")))
(check-true (decimal-zero? (d "0")))
(check-false (decimal-zero? (d "0.01")))

(check-catch 'type-error (decimal-zero? 0))

(check-report)
