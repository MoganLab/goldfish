;; decimal->byte
;; 转换为 byte（8位有符号整数，带舍入）。
;;
;; 语法
;; ----
;; (decimal->byte n)
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
;;   当值超出 [-128, 127] 范围时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

;; 正常范围
(check (decimal->byte (string->decimal "100")) => 100)
(check (decimal->byte (string->decimal "-50")) => -50)
(check (decimal->byte (string->decimal "127")) => 127)
(check (decimal->byte (string->decimal "-128")) => -128)

;; 超出范围报错
(check-catch 'value-error (decimal->byte (string->decimal "999")))
(check-catch 'value-error (decimal->byte (string->decimal "-999")))

(check-report)
