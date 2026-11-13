;; decimal->double
;; 将 decimal 转换为双精度浮点数。
;;
;; 语法
;; ----
;; (decimal->double n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; number?
;;   转换后的浮点数值。
;;
;; 说明
;; ----
;; 转换可能丢失精度，因为浮点数精度有限。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 基本转换
(check (decimal->double (string->decimal "3.14")) => 3.14)
(check (decimal->double (string->decimal "-123.456")) => -123.456)
(check (decimal->double (string->decimal "0")) => 0)

;; 整数（Scheme 中 100 和 100.0 是相同的值）
(check (decimal->double (string->decimal "100")) => 100)

(check-report)
