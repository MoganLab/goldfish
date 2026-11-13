;; math-context-rounding-mode
;; 获取 math-context 的舍入模式。
;;
;; 语法
;; ----
;; (math-context-rounding-mode mc)
;;
;; 参数
;; ----
;; mc : math-context?
;;   math-context 对象。
;;
;; 返回值
;; ----
;; symbol?
;;   舍入模式符号。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check (math-context-rounding-mode (make-math-context 10 'half-up)) => 'half-up)
(check (math-context-rounding-mode (make-math-context 5 'up)) => 'up)
(check (math-context-rounding-mode (make-math-context 5 'down)) => 'down)
(check (math-context-rounding-mode math-context-unlimited) => 'unnecessary)

(check-report)
