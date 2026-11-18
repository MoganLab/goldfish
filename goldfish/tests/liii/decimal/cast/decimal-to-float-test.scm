;; decimal->float
;; 转换为单精度浮点数（32位）。
;;
;; 语法
;; ----
;; (decimal->float n)
;;
;; 参数
;; ----
;; n : decimal?
;;   要转换的 decimal。
;;
;; 返回值
;; ----
;; real?
;;   对应的单精度浮点数，可能丢失精度。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是 decimal 时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

;; 基本转换
(check (decimal->float (string->decimal "3.14")) => 3.14)
(check (decimal->float (string->decimal "-123.456")) => -123.456)
(check (decimal->float (string->decimal "0")) => 0.0)

;; 整数
(check (decimal->float (string->decimal "100")) => 100.0)

;; 大数（可能丢失精度）
(check (real? (decimal->float (string->decimal "123456789.123456789"))) => #t)

(check-catch 'type-error (decimal->float "123"))

(check-report)
