;; decimal->plain-string
;; 返回无指数表示法的字符串（与 decimal->string 相同）。
;;
;; 语法
;; ----
;; (decimal->plain-string n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; string?
;;   纯字符串表示。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
(check (decimal->plain-string (string->decimal "123.456")) => "123.456")
(check (decimal->plain-string (string->decimal "0.001")) => "0.001")
(check (decimal->plain-string (string->decimal "-789")) => "-789")
(check (decimal->plain-string (string->decimal "0")) => "0")

;; 与 decimal->string 结果相同
(check (decimal->plain-string (string->decimal "123.456"))
       => (decimal->string (string->decimal "123.456")))

(check-report)
