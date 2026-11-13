;; decimal->long-exact
;; 精确转换，如果 decimal 有小数部分则报错。
;;
;; 语法
;; ----
;; (decimal->long-exact n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; integer?
;;   转换后的长整数值。
;;
;; 错误处理
;; ----
;; value-error
;;   当 decimal 有小数部分时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 精确整数可以转换
(check (decimal->long-exact (string->decimal "123")) => 123)
(check (decimal->long-exact (string->decimal "-456")) => -456)
(check (decimal->long-exact (string->decimal "0")) => 0)

;; 有小数部分的报错
(check-catch 'value-error (decimal->long-exact (string->decimal "123.5")))
(check-catch 'value-error (decimal->long-exact (string->decimal "0.1")))

(check-report)
