;; spinner-set-visible!
;; 设置微调器的可见性。
;;
;; 语法
;; ----
;; (spinner-set-visible! obj visible?)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; visible? : boolean?
;; 是否可见。
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
  (check (spinner-set-visible! s #f) => #t)
  (check-false (spinner-visible? s))
  (check (spinner-set-visible! s #t) => #t)
  (check-true (spinner-visible? s))
) ;let

(check-catch 'type-error (spinner-set-visible! "not-a-spinner" #t))

(check-report)
