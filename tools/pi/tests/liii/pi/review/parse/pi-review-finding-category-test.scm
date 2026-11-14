;; pi-review-finding-category
;; 获取代码审查问题记录的类别。
;;
;; 语法
;; ----
;; (pi-review-finding-category f)
;;
;; 参数
;; ----
;; f : pi-review-finding?
;; 代码审查问题记录。
;;
;; 返回值
;; ----
;; string
;; 问题类别。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check (pi-review-finding-category (make-pi-review-finding "HIGH" "a.scm" 1 "security" "msg" #f)) => "security")

(check-report)
