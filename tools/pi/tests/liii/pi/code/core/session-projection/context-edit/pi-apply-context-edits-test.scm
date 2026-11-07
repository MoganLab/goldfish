;; pi-apply-context-edits
;; 对上下文条目应用其中的撤销或替换编辑。
;;
;; 语法
;; ----
;; (pi-apply-context-edits context-entries)
;;
;; 参数
;; ----
;; context-entries : list
;; 上下文条目列表。
;;
;; 返回值
;; ----
;; 两个值
;; 应用编辑后的条目列表与被忽略的编辑 ID 列表。

(import (liii check)
  (liii pi code core session-projection context-edit)
) ;import

(check-set-mode! 'report-failed)

;; 1. 无编辑条目时原样返回
(let-values (((entries ignored) (pi-apply-context-edits '())))
  (check entries => '())
  (check ignored => '())
) ;let-values*

(check-report)
