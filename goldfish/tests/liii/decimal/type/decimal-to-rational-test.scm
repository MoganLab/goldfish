;; decimal->rational
;; 将 decimal 转换为有理数表示（分子 . 分母 的 pair）。
;;
;; 语法
;; ----
;; (decimal->rational n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal。
;;
;; 返回值
;; ----
;; pair?
;;   (分子 . 分母) 组成的对。

(import (liii check)
        (liii bigint)
        (liii decimal record)
        (liii decimal conversion)
) ;import

(check-set-mode! 'report-failed)

(check (bigint->string (car (decimal->rational (string->decimal "1.5")))) => "15")
(check (bigint->string (cdr (decimal->rational (string->decimal "1.5")))) => "10")

;; 负 scale（BUG-2）
(let ((rat (decimal->rational (make-decimal (integer->bigint 1) -3))))
  (check (bigint->string (car rat)) => "1000")
  (check (bigint->string (cdr rat)) => "1")
) ;let

(check-report)
