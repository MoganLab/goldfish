;; spinner-prefix
;; 获取微调器的前缀文本。
;;
;; 语法
;; ----
;; (spinner-prefix obj)
;;
;; 参数
;; ----
;; obj : spinner?
;; 微调器实例。
;;
;; 返回值
;; ----
;; string?
;; 前缀文本。

(import (scheme base)
  (liii check)
  (liii tui loader spinner)
  (liii tui loader state)
) ;import

(check-set-mode! 'report-failed)

(let ((s (make-spinner "Job" :prefix "[App] ")))
  (check (spinner-prefix s) => "[App] ")
) ;let

(check-catch 'type-error (spinner-prefix "not-a-spinner"))

(check-report)
