;; decimal->long
;; 将 decimal 转换为长整数（带舍入）。
;;
;; 语法
;; ----
;; (decimal->long n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; integer?
;;   转换后的长整数值（舍入到最近整数）。

(import (liii check)
        (liii bigint)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 整数转换
(check (decimal->long (string->decimal "123")) => 123)
(check (decimal->long (string->decimal "-456")) => -456)
(check (decimal->long (string->decimal "0")) => 0)

;; 负 scale（BUG-2）
(check (decimal->long (make-decimal (integer->bigint 1) -3)) => 1000)
(check (decimal->long (make-decimal (integer->bigint -5) -2)) => -500)

;; 带小数的舍入
(check (decimal->long (string->decimal "123.7")) => 123)
(check (decimal->long (string->decimal "123.2")) => 123)

;; 负数
(check (decimal->long (string->decimal "-789.5")) => -789)

(check-report)
