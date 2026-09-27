(import (liii check)
        (liii go))

(check-set-mode! 'report-failed)

;; context-done?
;; 判断 context 是否已完成（被取消或超时）。
;;
;; 语法
;; ----
;; (context-done? ctx)
;;
;; 参数
;; ----
;; ctx : context
;; 目标上下文。
;;
;; 返回值
;; ----
;; boolean
;; 已完成返回 #t，否则返回 #f。

(define ctx (make-context))
(check (context-done? ctx) => #f)
(context-cancel! ctx)
(check (context-done? ctx) => #t)

(check-report)
