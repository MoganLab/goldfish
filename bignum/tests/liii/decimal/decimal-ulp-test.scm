;; decimal-ulp
;; 返回 decimal 最后一位的单位大小（10^(-scale)）。
;;
;; 语法
;; ----
;; (decimal-ulp n)
;;
;; 参数
;; ----
;; n : decimal?
;;   输入 decimal。
;;
;; 返回值
;; ----
;; decimal?
;;   值为 10^(-scale) 的 decimal。
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

(check (decimal->string (decimal-ulp (d "1.23"))) => "0.01")
(check (decimal->string (decimal-ulp (d "100"))) => "1")
(check (decimal->string (decimal-ulp (decimal-set-scale (d "0") 4))) => "0.0001")
(check (decimal-scale (decimal-ulp (decimal-set-scale (d "0") 4))) => 4)

(check-catch 'type-error (decimal-ulp 1))

(check-report)
