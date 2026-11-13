;; decimal->string
;; 将 decimal 转换为字符串（无指数表示法）。
;;
;; 语法
;; ----
;; (decimal->string n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal。
;;
;; 返回值
;; ----
;; string?
;;   十进制字符串表示。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check (decimal->string (string->decimal "123")) => "123")
(check (decimal->string (string->decimal "0.000")) => "0.000")
(check (decimal->string (string->decimal "-0.050")) => "-0.050")
(check (decimal->string (string->decimal "1000000")) => "1000000")

(check-report)
