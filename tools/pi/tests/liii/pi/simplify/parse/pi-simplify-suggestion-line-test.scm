;; pi-simplify-suggestion-line
;; 获取代码简化建议记录的行号。
;;
;; 语法
;; ----
;; (pi-simplify-suggestion-line s)
;;
;; 参数
;; ----
;; s : pi-simplify-suggestion?
;; 简化建议记录。
;;
;; 返回值
;; ----
;; integer | #f
;; 行号或 #f。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(check (pi-simplify-suggestion-line (make-pi-simplify-suggestion "a.scm" 12 "cat" "msg" #f)) => 12)
(check (pi-simplify-suggestion-line (make-pi-simplify-suggestion "a.scm" #f "cat" "msg" #f)) => #f)

(check-report)
