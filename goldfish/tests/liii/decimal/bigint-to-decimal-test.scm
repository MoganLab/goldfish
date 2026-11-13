;; bigint->decimal
;; 将 bigint 转换为 decimal，可指定 scale。
;;
;; 语法
;; ----
;; (bigint->decimal n)
;; (bigint->decimal n scale)
;;
;; 参数
;; ----
;; n : bigint?
;;   要转换的 bigint 值。
;; scale : integer?
;;   可选参数，目标 scale，可以为任意整数。默认为 0。
;;
;; 返回值
;; ----
;; decimal?
;;   转换后的 decimal 值。
;;
;; 说明
;; ----
;; 该函数是 decimal-to-bigint 的逆操作。
;; 当指定 scale 时，相当于将 bigint 除以 10^scale。
;; 例如 (bigint->decimal (string->bigint "123") 2) => 1.23
;; 负 scale 表示放大：例如 (bigint->decimal (string->bigint "123") -2) => 12300
;;
;; 错误处理
;; ----
;; type-error
;;   当 n 不是 bigint，或 scale 不是整数时抛出。

(import (liii check)
        (liii decimal)
        (liii bigint)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
;; 默认 scale 为 0
(check (decimal->string (bigint->decimal (string->bigint "123"))) => "123")
(check (decimal->string (bigint->decimal (string->bigint "0"))) => "0")
(check (decimal->string (bigint->decimal (string->bigint "-456"))) => "-456")

;; 指定 scale
(check (decimal->string (bigint->decimal (string->bigint "123") 2)) => "1.23")
(check (decimal->string (bigint->decimal (string->bigint "12300") 2)) => "123.00")
(check (decimal->string (bigint->decimal (string->bigint "5") 1)) => "0.5")
(check (decimal->string (bigint->decimal (string->bigint "123456") 3)) => "123.456")

;; scale 为 0 时与默认值相同
(check (decimal->string (bigint->decimal (string->bigint "789") 0)) => "789")

;; 大数
(check (decimal->string (bigint->decimal (string->bigint "12345678901234567890") 10))
       => "1234567890.1234567890")

;; 零值
(check (decimal->string (bigint->decimal (string->bigint "0") 5)) => "0.00000")

;; scale 较大时
(check (decimal->string (bigint->decimal (string->bigint "1") 10)) => "0.0000000001")

;; 负 scale 支持
(check (decimal->string (bigint->decimal (string->bigint "123") -1)) => "1230")
(check (decimal->string (bigint->decimal (string->bigint "123") -2)) => "12300")
(check (decimal->string (bigint->decimal (string->bigint "5") -3)) => "5000")
(check (decimal->string (bigint->decimal (string->bigint "0") -2)) => "0")

;; 错误处理
(check-catch 'type-error (bigint->decimal 123))
(check-catch 'type-error (bigint->decimal "123"))
(check-catch 'type-error (bigint->decimal (string->bigint "123") 1.5))

(check-report)
