;; spinner-set-status!
;; 显式设置微调器的状态。
;;
;; 语法
;; ----
;; (spinner-set-status! obj status)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; status : symbol?
;; 目标状态符号。
;;
;; 返回值
;; ----
;; boolean?
;; 操作成功返回 #t。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job")))
  (check (spinner-set-status! s 'warning) => #t)
  (check (spinner-status s) => 'warning)
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-set-status! s 'invalid-status))
  (check-catch 'type-error (spinner-set-status! "not-a-spinner" 'success))
) ;let

(check-report)
