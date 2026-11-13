;; decimal-abs
;; 返回 decimal 的绝对值。
;;
;; 语法
;; ----
;; (decimal-abs n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; decimal?
;;   n 的绝对值。
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

(check (decimal->string (decimal-abs (d "-1.5"))) => "1.5")
(check (decimal->string (decimal-abs (d "1.5"))) => "1.5")
(check (decimal->string (decimal-abs (d "0"))) => "0")
(check (decimal-scale (decimal-abs (d "-2.50"))) => 2)

(check-catch 'type-error (decimal-abs 1))

(check-report)
