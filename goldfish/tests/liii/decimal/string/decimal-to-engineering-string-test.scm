;; decimal->engineering-string
;; 返回工程计数法字符串表示（指数是3的倍数）。
;;
;; 语法
;; ----
;; (decimal->engineering-string n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; string?
;;   工程计数法表示（指数是3的倍数）。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 基本测试 - 指数是3的倍数
(check (decimal->engineering-string (string->decimal "123.456")) => "123.456E+0")
(check (decimal->engineering-string (string->decimal "1230")) => "1.23E+3")
(check (decimal->engineering-string (string->decimal "1230000")) => "1.23E+6")

;; 零
(check (decimal->engineering-string (string->decimal "0")) => "0E+0")

;; 负数
(check (decimal->engineering-string (string->decimal "-1230000")) => "-1.23E+6")

;; 小数
(check (decimal->engineering-string (string->decimal "0.00123")) => "1.23E-3")

(check-report)
