;; decimal->int-exact
;; 精确转换为整数，有小数部分则报错。
;;
;; 语法
;; ----
;; (decimal->int-exact n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal。
;;
;; 返回值
;; ----
;; bigint?
;;   转换后的整数值。
;;
;; 错误处理
;; ----
;; value-error
;;   当值有小数部分时抛出。

(import (liii check)
        (liii bigint)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check (bigint->string (decimal->int-exact (string->decimal "123"))) => "123")
(check (bigint->string (decimal->int-exact (string->decimal "-456"))) => "-456")

(check-catch 'value-error (decimal->int-exact (string->decimal "123.5")))
(check-catch 'value-error (decimal->int-exact (string->decimal "1.000")))

(check-report)
