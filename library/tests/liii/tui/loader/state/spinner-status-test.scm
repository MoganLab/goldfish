;; spinner-status
;; 获取微调器当前的状态符号。
;;
;; 语法
;; ----
;; (spinner-status obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; symbol?
;; 当前状态，如 'spinning, 'success, 'error, 'warning, 'stopped。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Processing" :frames 'line)))
  (check (spinner-status s) => 'spinning)
  (spinner-success! s "Build passed")
  (check (spinner-status s) => 'success)
) ;let

(check-catch 'type-error (spinner-status "not-a-spinner"))

(check-report)
