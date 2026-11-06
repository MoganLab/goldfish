;; select-list-selected-index
;; 获取选择列表组件当前选中的条目索引（从 0 开始）。
;;
;; 语法
;; ----
;; (select-list-selected-index comp)
;;
;; 参数
;; ----
;; comp : select-list?
;; 选择列表组件实例。
;;
;; 返回值
;; ----
;; integer?
;; 当前选中条目的索引。

(import (scheme base) (liii check) (liii tui component) (liii tui select-list))

(check-set-mode! 'report-failed)

(let* ((items '((a) (b) (c))) (lst (make-select-list items 5)))
  (check (select-list-selected-index lst) => 0)
  (component-handle-input lst 'down)
  (check (select-list-selected-index lst) => 1)
  (component-handle-input lst 'down)
  (check (select-list-selected-index lst) => 2)
  (component-handle-input lst 'down)
  (check (select-list-selected-index lst) => 0)
) ;let*

(check-report)
