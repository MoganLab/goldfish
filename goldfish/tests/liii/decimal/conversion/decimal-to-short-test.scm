;; decimal->short
;; 转换为 short（16位有符号整数，带舍入）。
;;
;; 语法
;; ----
;; (decimal->short n)
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
;;   当值超出 [-32768, 32767] 范围时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

;; 正常范围
(check (decimal->short (string->decimal "100")) => 100)
(check (decimal->short (string->decimal "-50")) => -50)
(check (decimal->short (string->decimal "32767")) => 32767)
(check (decimal->short (string->decimal "-32768")) => -32768)

;; 超出范围报错
(check-catch 'value-error (decimal->short (string->decimal "99999")))
(check-catch 'value-error (decimal->short (string->decimal "-99999")))

(check-report)
