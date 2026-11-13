;; decimal-to-bigint
;; 将 decimal 转换为 bigint，截断小数部分。
;;
;; 语法
;; ----
;; (decimal-to-bigint n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal 值。
;;
;; 返回值
;; ----
;; bigint?
;;   decimal 的整数部分（截断小数），以 bigint 表示。
;;
;; 说明
;; ----
;; 该函数类似于 JDK BigDecimal 的 toBigInteger() 方法。
;; 截断是向零方向进行的（不是四舍五入）。
;; 例如 1.99 转换为 1，-1.99 转换为 -1。
;;
;; 错误处理
;; ----
;; type-error
;;   当输入不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
        (liii bigint)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(define (d str)
  (string->decimal str)
) ;define

;; 正数：截断小数部分
(check (bigint->string (decimal-to-bigint (d "1.23"))) => "1")
(check (bigint->string (decimal-to-bigint (d "1.99"))) => "1")
(check (bigint->string (decimal-to-bigint (d "123.456"))) => "123")
(check (bigint->string (decimal-to-bigint (d "0.999"))) => "0")

;; 负数：向零截断
(check (bigint->string (decimal-to-bigint (d "-1.23"))) => "-1")
(check (bigint->string (decimal-to-bigint (d "-1.99"))) => "-1")
(check (bigint->string (decimal-to-bigint (d "-123.456"))) => "-123")
(check (bigint->string (decimal-to-bigint (d "-0.999"))) => "0")

;; 整数：直接返回
(check (bigint->string (decimal-to-bigint (d "42"))) => "42")
(check (bigint->string (decimal-to-bigint (d "0"))) => "0")
(check (bigint->string (decimal-to-bigint (d "-42"))) => "-42")

;; 带 scale 的整数（如 1.000）
(check (bigint->string (decimal-to-bigint (decimal-set-scale (d "1") 3))) => "1")
(check (bigint->string (decimal-to-bigint (decimal-set-scale (d "0") 5))) => "0")

;; 大数
(check (bigint->string (decimal-to-bigint (d "12345678901234567890.123"))) => "12345678901234567890")
(check (bigint->string (decimal-to-bigint (d "-12345678901234567890.999"))) => "-12345678901234567890")

;; 负 scale（BUG-3）
(check (bigint->string (decimal-to-bigint (make-decimal (string->bigint "1") -3))) => "1000")
(check (bigint->string (decimal-to-bigint (make-decimal (string->bigint "-456") -2))) => "-45600")

;; 错误处理
(check-catch 'type-error (decimal-to-bigint 1))
(check-catch 'type-error (decimal-to-bigint "1.23"))
(check-catch 'type-error (decimal-to-bigint 'decimal))

(check-report)
