;; pi-review-finding-file
;; 获取代码审查问题记录的文件路径。
;;
;; 语法
;; ----
;; (pi-review-finding-file f)
;;
;; 参数
;; ----
;; f : pi-review-finding?
;; 代码审查问题记录。
;;
;; 返回值
;; ----
;; string
;; 文件路径。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(check (pi-review-finding-file (make-pi-review-finding "HIGH" "a.scm" 1 "cat" "msg" #f)) => "a.scm")

(check-report)
