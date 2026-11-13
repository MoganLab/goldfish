;; decimal->scientific-string
;; 返回科学计数法字符串表示。
;;
;; 语法
;; ----
;; (decimal->scientific-string n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; string?
;;   科学计数法表示（形式：mantissaE+/-exponent）。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 基本测试
(check (decimal->scientific-string (string->decimal "123.456")) => "1.23456E+2")
(check (decimal->scientific-string (string->decimal "0.00123")) => "1.23E-3")
(check (decimal->scientific-string (string->decimal "-456.789")) => "-4.56789E+2")

;; 零
(check (decimal->scientific-string (string->decimal "0")) => "0E+0")
(check (decimal->scientific-string (string->decimal "0.0")) => "0E+0")

;; 整数
(check (decimal->scientific-string (string->decimal "500")) => "5.00E+2")
(check (decimal->scientific-string (string->decimal "1")) => "1E+0")

;; 非常小的数
(check (decimal->scientific-string (string->decimal "0.00001")) => "1E-5")

;; 非常大的数
(check (decimal->scientific-string (string->decimal "123000000")) => "1.23000000E+8")

(check-report)
