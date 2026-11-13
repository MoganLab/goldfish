;; math-context-decimal128
;; 标准常量：34 位精度，half-even 舍入。
;;
;; 语法
;; ----
;; math-context-decimal128
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; math-context?
;;   IEEE 754-2008 decimal128 对应的上下文。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check (math-context? math-context-decimal128) => #t)
(check (math-context-precision math-context-decimal128) => 34)
(check (math-context-rounding-mode math-context-decimal128) => 'half-even)

(check-report)
