;; math-context-valid-rounding-mode?
;; 检查舍入模式是否有效，有效时返回非 #f 的候选列表。
;;
;; 语法
;; ----
;; (math-context-valid-rounding-mode? mode)
;;
;; 参数
;; ----
;; mode : any
;;   待检查的值。
;;
;; 返回值
;; ----
;; list? 或 #f
;;   有效时返回以 mode 开头的候选列表，无效返回 #f（memq 风格）。

(import (liii check)
        (liii decimal math-context)
) ;import

(check-set-mode! 'report-failed)

(check-true (not (not (math-context-valid-rounding-mode? 'up))))
(check-true (not (not (math-context-valid-rounding-mode? 'half-even))))
(check-false (math-context-valid-rounding-mode? 'invalid))
(check-false (math-context-valid-rounding-mode? "half-up"))

(check-report)
