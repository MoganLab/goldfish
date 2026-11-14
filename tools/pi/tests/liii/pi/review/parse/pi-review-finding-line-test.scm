;; pi-review-finding-line
;; 获取代码审查问题记录的行号。
;;
;; 语法
;; ----
;; (pi-review-finding-line f)
;;
;; 参数
;; ----
;; f : pi-review-finding?
;; 代码审查问题记录。
;;
;; 返回值
;; ----
;; integer | #f
;; 行号或 #f。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check (pi-review-finding-line (make-pi-review-finding "HIGH" "a.scm" 42 "cat" "msg" #f)) => 42)
(check (pi-review-finding-line (make-pi-review-finding "HIGH" "a.scm" #f "cat" "msg" #f)) => #f)

(check-report)
