;; spinner-text
;; 获取微调器的提示文本。
;;
;; 语法
;; ----
;; (spinner-text obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; string?
;; 提示文本。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Initial")))
  (check (spinner-text s) => "Initial")
  (spinner-set-text! s "Updated")
  (check (spinner-text s) => "Updated")
) ;let

(check-catch 'type-error (spinner-text "not-a-spinner"))

(check-report)
