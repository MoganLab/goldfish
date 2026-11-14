;; pi-simplify-suggestion-message
;; 获取代码简化建议记录的描述信息。
;;
;; 语法
;; ----
;; (pi-simplify-suggestion-message s)
;;
;; 参数
;; ----
;; s : pi-simplify-suggestion?
;; 简化建议记录。
;;
;; 返回值
;; ----
;; string
;; 描述信息。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(check (pi-simplify-suggestion-message (make-pi-simplify-suggestion "a.scm" 1 "naming" "rename x" #f)) => "rename x")

(check-report)
