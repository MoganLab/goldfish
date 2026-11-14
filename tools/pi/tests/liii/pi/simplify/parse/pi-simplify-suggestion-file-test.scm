;; pi-simplify-suggestion-file
;; 获取代码简化建议记录的文件路径。
;;
;; 语法
;; ----
;; (pi-simplify-suggestion-file s)
;;
;; 参数
;; ----
;; s : pi-simplify-suggestion?
;; 简化建议记录。
;;
;; 返回值
;; ----
;; string
;; 文件路径。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(check (pi-simplify-suggestion-file (make-pi-simplify-suggestion "a.scm" 1 "cat" "msg" #f)) => "a.scm")

(check-report)
