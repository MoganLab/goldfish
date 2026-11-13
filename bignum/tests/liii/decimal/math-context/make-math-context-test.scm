;; make-math-context
;; 创建 math-context 对象。
;;
;; 语法
;; ----
;; (make-math-context precision rounding-mode)
;;
;; 参数
;; ----
;; precision : non-negative-integer?
;;   精度设置，0 表示无限精度。
;; rounding-mode : symbol?
;;   舍入模式：'up, 'down, 'ceiling, 'floor, 'half-up, 'half-down, 'half-even, 'unnecessary。
;;
;; 返回值
;; ----
;; math-context?
;;   新创建的 math-context 对象。
;;
;; 错误处理
;; ----
;; value-error
;;   当精度为负或舍入模式无效时抛出。
;; type-error
;;   当参数类型不正确时抛出。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(let ((mc (make-math-context 10 'half-up)))
  (check (math-context-precision mc) => 10)
  (check (math-context-rounding-mode mc) => 'half-up)
) ;let

(check-catch 'value-error (make-math-context -1 'half-up))
(check-catch 'type-error (make-math-context 10.5 'half-up))
(check-catch 'value-error (make-math-context 10 "half-up"))
(check-catch 'value-error (make-math-context 10 'invalid-mode))

(check-report)
