;; reader-skip-selected
;; 跳过已选中的表单，返回剩余部分。
;;
;; 语法
;; ----
;; (reader-skip-selected rest selected)
;;
;; 参数
;; ----
;; rest : list?
;; 待处理的表单列表。
;;
;; selected : list?
;; 已选中的项列表。
;;
;; 返回值
;; ----
;; list?
;; 跳过 selected 个元素后的剩余列表。

(import (liii check) (liii goldfmt format datum))

(check-set-mode! 'report-failed)

(check (reader-skip-selected '(a b c) '((a) (b))) => '(c))
(check (reader-skip-selected '(a) '()) => '(a))

(check-report)
