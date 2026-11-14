;; make-pi-review-finding
;; 构造代码审查问题记录。
;;
;; 语法
;; ----
;; (make-pi-review-finding severity file line category message suggestion)
;;
;; 参数
;; ----
;; severity : string
;; 严重程度。
;; file : string
;; 目标文件路径。
;; line : integer | #f
;; 行号或 #f。
;; category : string
;; 问题类别。
;; message : string
;; 问题描述信息。
;; suggestion : string | #f
;; 修复建议或 #f。
;;
;; 返回值
;; ----
;; pi-review-finding?
;; 代码审查问题记录。

(import (scheme base) (liii check) (liii pi review parse))

(check-set-mode! 'report-failed)

(let ((f (make-pi-review-finding "HIGH" "a.scm" 42 "security" "bad" "fix")))
  (check-true (pi-review-finding? f))
  (check-false (pi-review-finding? '()))
  (check (pi-review-finding-severity f) => "HIGH")
  (check (pi-review-finding-file f) => "a.scm")
  (check (pi-review-finding-line f) => 42)
  (check (pi-review-finding-category f) => "security")
  (check (pi-review-finding-message f) => "bad")
  (check (pi-review-finding-suggestion f) => "fix")
) ;let

(check-report)
