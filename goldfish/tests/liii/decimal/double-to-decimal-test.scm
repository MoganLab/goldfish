;; double->decimal
;; 将实数转换为 decimal。
;;
;; 语法
;; ----
;; (double->decimal n)
;; (double->decimal n scale)
;;
;; 参数
;; ----
;; n : real?
;;   实数（浮点数）。
;; scale : integer?
;;   可选，指定 scale，默认为 16（对应 double 精度）。
;;
;; 返回值
;; ----
;; decimal?
;;   转换后的 decimal 值。
;;
;; 说明
;; ----
;; 由于浮点数的精度限制，转换可能不完全精确。
;; 建议使用 string->decimal 从字符串构造精确的 decimal。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 基本转换
(check (decimal->string (double->decimal 1.5)) => "1.5")
(check (decimal->string (double->decimal 0.0)) => "0.0")
(check (decimal->string (double->decimal -3.14)) => "-3.14")
(check (decimal->string (double->decimal 123.456)) => "123.456")

;; 整数转换（作为 double 传入）
(check (decimal->string (double->decimal 100.0)) => "100.0")
(check (decimal->string (double->decimal -50.0)) => "-50.0")

;; 指定 scale 测试（BUG-9）
(check (decimal->string (double->decimal 3.14 0)) => "3")
(check (decimal->string (double->decimal 3.14 1)) => "3.1")
(check (decimal->string (double->decimal 3.14 4)) => "3.1400")

;; 极大 double 科学计数法转换（BUG-7 联动）
(check (decimal? (double->decimal 1e21)) => #t)

;; 错误处理
(check-catch 'type-error (double->decimal "1.5"))    ; 字符串
(check-catch 'type-error (double->decimal 'symbol))  ; 符号

(check-report)
