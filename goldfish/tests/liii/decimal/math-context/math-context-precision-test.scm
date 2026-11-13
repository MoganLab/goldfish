;; math-context-precision
;; 获取 math-context 的精度。
;;
;; 语法
;; ----
;; (math-context-precision mc)
;;
;; 参数
;; ----
;; mc : math-context?
;;   math-context 对象。
;;
;; 返回值
;; ----
;; integer?
;;   精度值，0 表示无限精度。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check (math-context-precision (make-math-context 10 'half-up)) => 10)
(check (math-context-precision (make-math-context 0 'half-up)) => 0)
(check (math-context-precision math-context-decimal32) => 7)
(check (math-context-precision math-context-unlimited) => 0)

(check-report)
