;; decimal-to-bigint-exact
;; 精确转换为 bigint，如果有小数部分则报错。
;;
;; 语法
;; ----
;; (decimal-to-bigint-exact n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; bigint?
;;   转换后的 bigint 值。
;;
;; 错误处理
;; ----
;; value-error
;;   当 decimal 有小数部分时抛出。

(import (liii check)
        (liii decimal)
        (liii bigint)
) ;import

(check-set-mode! 'report-failed)
;; 精确整数可以转换
(check (bigint=? (decimal-to-bigint-exact (string->decimal "123"))
                 (string->bigint "123")) => #t)
(check (bigint=? (decimal-to-bigint-exact (string->decimal "-456"))
                 (string->bigint "-456")) => #t)
(check (bigint=? (decimal-to-bigint-exact (string->decimal "0"))
                 (string->bigint "0")) => #t)

;; 大整数
(check (bigint=? (decimal-to-bigint-exact (string->decimal "999999999999999999999999999999"))
                 (string->bigint "999999999999999999999999999999")) => #t)

;; 负 scale（BUG-4）
(check (bigint=? (decimal-to-bigint-exact (make-decimal (string->bigint "1") -3))
                 (string->bigint "1000")) => #t)
(check (bigint=? (decimal-to-bigint-exact (make-decimal (string->bigint "-45") -2))
                 (string->bigint "-4500")) => #t)

;; scale > 0 但小数部分全为 0（能整除）
(check (bigint=? (decimal-to-bigint-exact (string->decimal "123.000"))
                 (string->bigint "123")) => #t)
(check (bigint=? (decimal-to-bigint-exact (string->decimal "-456.0"))
                 (string->bigint "-456")) => #t)

;; 有小数部分的报错
(check-catch 'value-error (decimal-to-bigint-exact (string->decimal "123.5")))
(check-catch 'value-error (decimal-to-bigint-exact (string->decimal "0.1")))
(check-catch 'value-error (decimal-to-bigint-exact (string->decimal "-456.789")))

;; 类型错误
(check-catch 'type-error (decimal-to-bigint-exact "123"))

(check-report)
