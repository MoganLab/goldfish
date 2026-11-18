;; decimal->byte-exact
;; 精确转换为 byte，有小数部分则报错。
;;
;; 语法
;; ----
;; (decimal->byte-exact n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal。
;;
;; 返回值
;; ----
;; integer?
;;   转换后的 byte 值。
;;
;; 错误处理
;; ----
;; value-error
;;   当值有小数部分或超出范围时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check (decimal->byte-exact (string->decimal "123")) => 123)
(check (decimal->byte-exact (string->decimal "-128")) => -128)

(check-catch 'value-error (decimal->byte-exact (string->decimal "123.5")))
(check-catch 'value-error (decimal->byte-exact (string->decimal "999")))

(check-report)
