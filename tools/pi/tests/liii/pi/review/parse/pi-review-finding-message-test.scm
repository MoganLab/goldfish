;; pi-review-finding-message
;; 获取代码审查问题记录的描述信息。
;;
;; 语法
;; ----
;; (pi-review-finding-message f)
;;
;; 参数
;; ----
;; f : pi-review-finding?
;; 代码审查问题记录。
;;
;; 返回值
;; ----
;; string
;; 描述信息。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check (pi-review-finding-message (make-pi-review-finding "HIGH" "a.scm" 1 "security" "bad" #f)) => "bad")

(check-report)
