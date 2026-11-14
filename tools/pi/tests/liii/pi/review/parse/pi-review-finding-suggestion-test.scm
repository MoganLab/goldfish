;; pi-review-finding-suggestion
;; 获取代码审查问题记录的修复建议。
;;
;; 语法
;; ----
;; (pi-review-finding-suggestion f)
;;
;; 参数
;; ----
;; f : pi-review-finding?
;; 代码审查问题记录。
;;
;; 返回值
;; ----
;; string | #f
;; 修复建议或 #f。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check (pi-review-finding-suggestion (make-pi-review-finding "HIGH" "a.scm" 1 "security" "bad" "fix")) => "fix")
(check (pi-review-finding-suggestion (make-pi-review-finding "HIGH" "a.scm" 1 "security" "bad" #f)) => #f)

(check-report)
