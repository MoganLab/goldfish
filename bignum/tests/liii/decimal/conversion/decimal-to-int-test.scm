;; decimal->int
;; 将 decimal 转换为整数（带舍入），检查范围。
;;
;; 语法
;; ----
;; (decimal->int n)
;;
;; 参数
;; ----
;; n : decimal?
;;   decimal 值。
;;
;; 返回值
;; ----
;; integer?
;;   转换后的整数值（舍入到最近整数）。
;;
;; 错误处理
;; ----
;; value-error
;;   当值超出 int 范围时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)
;; 正常范围
(check (decimal->int (string->decimal "100")) => 100)
(check (decimal->int (string->decimal "-50")) => -50)
(check (decimal->int (string->decimal "2147483647")) => 2147483647)   ; max int
(check (decimal->int (string->decimal "-2147483648")) => -2147483648) ; min int

;; 超出范围报错
(check-catch 'value-error (decimal->int (string->decimal "9999999999999")))
(check-catch 'value-error (decimal->int (string->decimal "-9999999999999")))

(check-report)
