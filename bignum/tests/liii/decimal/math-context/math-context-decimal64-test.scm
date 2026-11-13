;; math-context-decimal64
;; 标准常量：16 位精度，half-even 舍入。
;;
;; 语法
;; ----
;; math-context-decimal64
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; math-context?
;;   IEEE 754-2008 decimal64 对应的上下文。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check (math-context? math-context-decimal64) => #t)
(check (math-context-precision math-context-decimal64) => 16)
(check (math-context-rounding-mode math-context-decimal64) => 'half-even)

(check-report)
