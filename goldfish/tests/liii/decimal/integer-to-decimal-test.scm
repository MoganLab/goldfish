;; integer->decimal
;; 将 Scheme 整数转换为 decimal。
;;
;; 语法
;; ----
;; (integer->decimal n)
;; (integer->decimal n scale)
;;
;; 参数
;; ----
;; n : integer?
;;   Scheme 整数（fixnum 或 bignum）。
;; scale : integer?
;;   可选，指定 scale，默认为 0。
;;
;; 返回值
;; ----
;; decimal?
;;   转换后的 decimal 值。
;;
;; 说明
;; ----
;; integer->decimal 将 Scheme 整数转换为 decimal。
;; 如果提供了 scale 参数，则使用指定的 scale；否则 scale 为 0。

(import (liii check)
        (liii decimal)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
;; 基本转换（无 scale 参数，默认为 0）
(check (decimal->string (integer->decimal 123)) => "123")
(check (decimal->string (integer->decimal 0)) => "0")
(check (decimal->string (integer->decimal -456)) => "-456")
(check (decimal-scale (integer->decimal 123)) => 0)

;; 带 scale 参数的转换
(check (decimal->string (integer->decimal 123 2)) => "1.23")
(check (decimal->string (integer->decimal 12300 2)) => "123.00")
(check (decimal->string (integer->decimal 5 1)) => "0.5")
(check (decimal->string (integer->decimal 123456 3)) => "123.456")
(check (decimal-scale (integer->decimal 123 2)) => 2)

;; scale 为 0
(check (decimal->string (integer->decimal 789 0)) => "789")
(check (decimal-scale (integer->decimal 789 0)) => 0)

;; 负 scale（放大数值）
(check (decimal->string (integer->decimal 123 -1)) => "1230")
(check (decimal->string (integer->decimal 123 -2)) => "12300")
(check (decimal->string (integer->decimal 5 -3)) => "5000")
(check (decimal->string (integer->decimal 0 -2)) => "0")
(check (decimal-scale (integer->decimal 123 -2)) => -2)

;; 零值的各种 scale
(check (decimal->string (integer->decimal 0 0)) => "0")
(check (decimal->string (integer->decimal 0 5)) => "0.00000")
(check (decimal->string (integer->decimal 0 10)) => "0.0000000000")
(check (decimal-scale (integer->decimal 0 5)) => 5)

;; 大整数
(check (decimal->string (integer->decimal 1234567890 8))
       => "12.34567890")

;; 与 string->decimal 对比（应该相等）
(check (decimal=? (integer->decimal 123) (string->decimal "123")) => #t)
(check (decimal=? (integer->decimal 123 2) (string->decimal "1.23")) => #t)
(check (decimal=? (integer->decimal -456 1) (string->decimal "-45.6")) => #t)

;; 错误处理
(check-catch 'type-error (integer->decimal 123.45))     ; 浮点数
(check-catch 'type-error (integer->decimal "123"))      ; 字符串
(check-catch 'type-error (integer->decimal 'symbol))    ; 符号
(check-catch 'type-error (integer->decimal 123 1.5))    ; scale 不是整数

(check-report)
