;; spinner-hide!
;; 隐藏微调器。
;;
;; 语法
;; ----
;; (spinner-hide! obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
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
  (check-true (spinner-visible? s))
  (check (spinner-hide! s) => #t)
  (check-false (spinner-visible? s))
) ;let

(check-catch 'type-error (spinner-hide! "not-a-spinner"))

(check-report)
