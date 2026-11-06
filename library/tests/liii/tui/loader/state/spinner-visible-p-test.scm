;; spinner-visible?
;; 查询微调器是否处于可见状态。
;;
;; 语法
;; ----
;; (spinner-visible? obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; boolean?
;; 可见返回 #t，隐藏返回 #f。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job")))
  (check-true (spinner-visible? s))
  (spinner-hide! s)
  (check-false (spinner-visible? s))
  (spinner-show! s)
  (check-true (spinner-visible? s))
) ;let

(check-catch 'type-error (spinner-visible? "not-a-spinner"))

(check-report)
