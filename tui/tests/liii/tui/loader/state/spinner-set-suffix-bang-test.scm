;; spinner-set-suffix!
;; 动态设置微调器的后缀文本。
;;
;; 语法
;; ----
;; (spinner-set-suffix! obj suffix)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; suffix : string?
;; 新的后缀文本。
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
  (check (spinner-set-suffix! s " 100%") => #t)
  (check (spinner-suffix s) => " 100%")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-set-suffix! s 123))
  (check-catch 'type-error (spinner-set-suffix! "not-a-spinner" "ok"))
) ;let

(check-report)
