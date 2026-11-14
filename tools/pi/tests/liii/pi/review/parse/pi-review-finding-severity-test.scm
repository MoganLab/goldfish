;; pi-review-finding-severity
;; 获取代码审查问题记录的严重程度。
;;
;; 语法
;; ----
;; (pi-review-finding-severity f)
;;
;; 参数
;; ----
;; f : pi-review-finding?
;; 代码审查问题记录。
;;
;; 返回值
;; ----
;; string
;; 严重程度。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check (pi-review-finding-severity (make-pi-review-finding "HIGH" "a.scm" 1 "cat" "msg" #f)) => "HIGH")

(check-report)
