;; spinner-suffix
;; 获取微调器的后缀文本。
;;
;; 语法
;; ----
;; (spinner-suffix obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; string?
;; 后缀文本。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job" :suffix " 45%")))
  (check (spinner-suffix s) => " 45%")
) ;let

(check-catch 'type-error (spinner-suffix "not-a-spinner"))

(check-report)
