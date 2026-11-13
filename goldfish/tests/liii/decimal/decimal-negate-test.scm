;; decimal-negate
;; 返回 decimal 的相反数。
;;
;; 语法
;; ----
;; (decimal-negate n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; decimal?
;;   n 的相反数。
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

(check (decimal->string (decimal-negate (d "1.5"))) => "-1.5")
(check (decimal->string (decimal-negate (d "-1.5"))) => "1.5")
(check (decimal->string (decimal-negate (d "0"))) => "0")

(check-catch 'type-error (decimal-negate 1))

(check-report)
