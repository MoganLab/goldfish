;; long->decimal
;; 将长整数转换为 decimal。
;;
;; 语法
;; ----
;; (long->decimal n)
;; (long->decimal n scale)
;;
;; 参数
;; ----
;; n : integer?
;;   长整数值。
;; scale : integer?
;;   可选的 scale，默认 0。
;;
;; 返回值
;; ----
;; decimal?
;;   对应的 decimal。

(import (liii check)
        (liii bigint)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check (decimal->string (long->decimal 123456789012345)) => "123456789012345")
(check (decimal->string (long->decimal -42)) => "-42")
(check (decimal-scale (long->decimal 5)) => 0)
(check (decimal->string (long->decimal 1234 2)) => "12.34")

(check-catch 'type-error (long->decimal "123"))

(check-report)
