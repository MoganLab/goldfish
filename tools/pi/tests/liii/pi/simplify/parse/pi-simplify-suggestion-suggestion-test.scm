;; pi-simplify-suggestion-suggestion
;; 获取代码简化建议记录的具体修改建议。
;;
;; 语法
;; ----
;; (pi-simplify-suggestion-suggestion s)
;;
;; 参数
;; ----
;; s : pi-simplify-suggestion?
;; 简化建议记录。
;;
;; 返回值
;; ----
;; string | #f
;; 具体修改建议或 #f。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(check (pi-simplify-suggestion-suggestion (make-pi-simplify-suggestion "a.scm" 1 "naming" "msg" "use count")) => "use count")
(check (pi-simplify-suggestion-suggestion (make-pi-simplify-suggestion "a.scm" 1 "naming" "msg" #f)) => #f)

(check-report)
