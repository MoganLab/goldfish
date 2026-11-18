;; decimal->short-exact
;; 精确转换为 short，有小数部分则报错。
;;
;; 语法
;; ----
;; (decimal->short-exact n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal。
;;
;; 返回值
;; ----
;; integer?
;;   转换后的 short 值。
;;
;; 错误处理
;; ----
;; value-error
;;   当值有小数部分或超出范围时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check (decimal->short-exact (string->decimal "123")) => 123)
(check (decimal->short-exact (string->decimal "-32768")) => -32768)

(check-catch 'value-error (decimal->short-exact (string->decimal "123.5")))
(check-catch 'value-error (decimal->short-exact (string->decimal "99999")))

(check-report)
