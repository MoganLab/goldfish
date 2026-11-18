;; integer->bigint
;; 将精确整数或已有 bigint 转换为 bigint。
;;
;; 语法
;; ----
;; (integer->bigint n)
;;
;; 参数
;; ----
;; n : integer? or bigint?
;;   要转换的整数值。
;;
;; 返回值
;; ----
;; bigint?
;;   转换结果。
;;
;; 错误处理
;; ----
;; type-error
;;   当参数不是精确整数也不是 bigint 时抛出。

(import (liii check)
        (liii bigint conversion)
) ;import

(check-set-mode! 'report-failed)
(check (bigint->string (integer->bigint 0)) => "0")
(check (bigint->string (integer->bigint 12345)) => "12345")
(check (bigint->string (integer->bigint -9876)) => "-9876")
(check (bigint->string (integer->bigint (string->bigint "99"))) => "99")

(check-catch 'type-error (integer->bigint "123"))
(check-catch 'type-error (integer->bigint 1.5))

(check-report)
