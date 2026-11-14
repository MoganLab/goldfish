;; make-pi-simplify-suggestion
;; 构造代码简化建议记录。
;;
;; 语法
;; ----
;; (make-pi-simplify-suggestion file line category message suggestion)
;;
;; 参数
;; ----
;; file : string
;; 目标文件路径。
;; line : integer | #f
;; 行号或 #f。
;; category : string
;; 建议类别。
;; message : string
;; 建议描述信息。
;; suggestion : string | #f
;; 具体修改建议或 #f。
;;
;; 返回值
;; ----
;; pi-simplify-suggestion?
;; 简化建议记录。

(import (scheme base) (liii check) (liii pi simplify parse))

(check-set-mode! 'report-failed)

(let ((s (make-pi-simplify-suggestion "a.scm" 10 "naming" "rename x" "use count")))
  (check-true (pi-simplify-suggestion? s))
  (check-false (pi-simplify-suggestion? '()))
  (check (pi-simplify-suggestion-file s) => "a.scm")
  (check (pi-simplify-suggestion-line s) => 10)
  (check (pi-simplify-suggestion-category s) => "naming")
  (check (pi-simplify-suggestion-message s) => "rename x")
  (check (pi-simplify-suggestion-suggestion s) => "use count")
) ;let

(check-report)
