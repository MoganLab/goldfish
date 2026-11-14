;; pi-simplify-suggestion-category
;; 获取代码简化建议记录的类别。
;;
;; 语法
;; ----
;; (pi-simplify-suggestion-category s)
;;
;; 参数
;; ----
;; s : pi-simplify-suggestion?
;; 简化建议记录。
;;
;; 返回值
;; ----
;; string
;; 建议类别。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(check (pi-simplify-suggestion-category (make-pi-simplify-suggestion "a.scm" 1 "naming" "msg" #f)) => "naming")

(check-report)
