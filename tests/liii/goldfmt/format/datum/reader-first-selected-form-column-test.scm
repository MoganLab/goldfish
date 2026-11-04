;; reader-first-selected-form-column
;; 查找首个选中列表单的目标列号。
;;
;; 语法
;; ----
;; (reader-first-selected-form-column selected)
;;
;; 参数
;; ----
;; selected : list?
;; 选中项列表。
;;
;; 返回值
;; ----
;; integer? 或 #f
;; 首个选中列表单的列号，不存在时返回 #f。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-first-selected-form-column '()) => #f)
(check (reader-first-selected-form-column (list (list 'atom 3 "x"))) => #f)
(check
  (reader-first-selected-form-column (list (list '(f) 4 "x")))
  =>
  4
) ;check

(check-report)
