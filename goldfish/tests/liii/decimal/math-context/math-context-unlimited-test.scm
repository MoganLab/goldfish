;; math-context-unlimited
;; 标准常量：无限精度（0），unnecessary 舍入。
;;
;; 语法
;; ----
;; math-context-unlimited
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; math-context?
;;   无限精度的上下文。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check (math-context? math-context-unlimited) => #t)
(check (math-context-precision math-context-unlimited) => 0)
(check (math-context-rounding-mode math-context-unlimited) => 'unnecessary)

(check-report)
