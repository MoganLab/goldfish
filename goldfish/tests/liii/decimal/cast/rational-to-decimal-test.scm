;; rational->decimal
;; 将有理数（分子/分母）转换为 decimal。
;;
;; 语法
;; ----
;; (rational->decimal numerator denominator)
;; (rational->decimal numerator denominator scale rounding-mode)
;;
;; 参数
;; ----
;; numerator : integer?
;;   分子。
;; denominator : integer?
;;   分母，不能为零。
;; scale : integer?
;;   可选的目标 scale。
;; rounding-mode : symbol?
;;   可选的舍入模式。
;;
;; 返回值
;; ----
;; decimal?
;;   对应的 decimal。
;;
;; 错误处理
;; ----
;; value-error
;;   当分母为零时抛出。
;; type-error
;;   当分子或分母不是整数时抛出。

(import (liii check)
        (liii decimal)
) ;import

(check-set-mode! 'report-failed)

(check (decimal->string (rational->decimal 1 2)) => "1")
(check (decimal->string (rational->decimal 22 7 3 'half-up)) => "3.143")
(check (decimal->string (rational->decimal 1 2 4 'half-up)) => "0.5000")
(check (decimal->string (rational->decimal -1 4 2 'half-up)) => "-0.25")

(check-catch 'value-error (rational->decimal 1 0))
(check-catch 'type-error (rational->decimal 1.5 2))

(check-report)
