;; string->decimal
;; 从十进制字符串构造 decimal。
;;
;; 语法
;; ----
;; (string->decimal str)
;;
;; 参数
;; ----
;; str : string?
;;   十进制字符串，可带符号与小数点。
;;
;; 返回值
;; ----
;; decimal?
;;   对应的 decimal，末尾零被保留在 scale 中。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是字符串时抛出。
;; value-error
;;   当字符串为空或格式非法时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

;; 基本转换
(check (decimal->string (string->decimal "123")) => "123")
(check (decimal->string (string->decimal ".5")) => "0.5")
(check (decimal->string (string->decimal "10.")) => "10")

;; scale 保留：输入中的末尾零被保留
(check (decimal->string (string->decimal "1.2300")) => "1.2300")
(check (decimal->string (string->decimal "-0.050")) => "-0.050")

;; 验证 scale
(check (decimal-scale (string->decimal "1.23")) => 2)
(check (decimal-scale (string->decimal "1.2300")) => 4)

;; 科学记数法解析（BUG-7）
(check (decimal->string (string->decimal "1E+3")) => "1000")
(check (decimal-scale (string->decimal "1E+3")) => -3)
(check (decimal->string (string->decimal "1.23E4")) => "12300")
(check (decimal-scale (string->decimal "1.23E4")) => -2)
(check (decimal->string (string->decimal "1.23E-2")) => "0.0123")
(check (decimal-scale (string->decimal "1.23E-2")) => 4)
(check (decimal->string (string->decimal "1E-3")) => "0.001")
(check (decimal-scale (string->decimal "1E-3")) => 3)
(check (decimal->string (string->decimal "-2.5e3")) => "-2500")

;; 往返：scientific-string -> decimal -> string
(check (decimal->string
        (string->decimal (decimal->scientific-string (string->decimal "12300"))))
       => "12300")

(check-catch 'type-error (string->decimal 1))
(check-catch 'value-error (string->decimal ""))
(check-catch 'value-error (string->decimal "."))
(check-catch 'value-error (string->decimal "1.2.3"))
(check-catch 'value-error (string->decimal "1e"))
(check-catch 'value-error (string->decimal "1e+"))
(check-catch 'value-error (string->decimal "1.2e3e4"))
(check-catch 'value-error (string->decimal "abc"))

(check-report)
