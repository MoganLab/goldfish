;; spinner-set-text!
;; 动态设置微调器的提示文本。
;;
;; 语法
;; ----
;; (spinner-set-text! obj text)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; text : string?
;; 新的提示文本。
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

(let ((s (make-spinner "Old")))
  (check (spinner-set-text! s "New") => #t)
  (check (spinner-text s) => "New")
) ;let

(let ((s (make-spinner)))
  (check-catch 'type-error (spinner-set-text! s 123))
  (check-catch 'type-error (spinner-set-text! "not-a-spinner" "ok"))
) ;let

(check-report)
