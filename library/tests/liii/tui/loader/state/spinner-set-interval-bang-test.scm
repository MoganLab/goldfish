;; spinner-set-interval!
;; 设置微调器的刷新间隔。
;;
;; 语法
;; ----
;; (spinner-set-interval! obj interval)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; interval : integer?
;; 新的刷新间隔（非负整数）。
;;
;; 返回值
;; ----
;; boolean?
;; 设置成功返回 #t。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job")))
  (check (spinner-set-interval! s 60) => #t)
  (check (spinner-interval s) => 60)
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-set-interval! s -1))
  (check-catch 'type-error (spinner-set-interval! s "60"))
  (check-catch 'type-error (spinner-set-interval! "not-a-spinner" 60))
) ;let

(check-report)
