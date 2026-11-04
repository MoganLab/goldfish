;; selected-count
;; 统计选中项的数量。
;;
;; 语法
;; ----
;; (selected-count selected)
;;
;; 参数
;; ----
;; selected : list?
;; 由 `select-first-line-children` 返回的选中项列表。
;;
;; 返回值
;; ----
;; integer?
;; 选中项的数量。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

(let* ((node (scan '(+ 1 2))) (selected (select-first-line-children node 1)))
  (check (selected-count selected) => 1)
) ;let*

;; 测试：没有子节点时数量为 0
(let* ((node (scan '(+))) (selected (select-first-line-children node 1)))
  (check (selected-count selected) => 0)
) ;let*

(check-report)
