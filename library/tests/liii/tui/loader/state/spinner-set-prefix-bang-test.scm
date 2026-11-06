;; spinner-set-prefix!
;; 动态设置微调器的前缀文本。
;;
;; 语法
;; ----
;; (spinner-set-prefix! obj prefix)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; prefix : string?
;; 新的前缀文本。
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
  (check (spinner-set-prefix! s "[New] ") => #t)
  (check (spinner-prefix s) => "[New] ")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-set-prefix! s 123))
  (check-catch 'type-error (spinner-set-prefix! "not-a-spinner" "ok"))
) ;let

(check-report)
