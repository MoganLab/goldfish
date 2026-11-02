;; select-list-selected-item
;; 获取选择列表组件当前被选中的条目。
;;
;; 语法
;; ----
;; (select-list-selected-item comp)
;;
;; 参数
;; ----
;; comp : select-list?
;; 选择列表组件实例。
;;
;; 返回值
;; ----
;; any 或 #f
;; 当前选中的条目；若列表为空则返回 #f。

(import (scheme base) (liii check) (liii tui component) (liii tui select-list))

(check-set-mode! 'report-failed)

(let* ((items '(("first" . 1) ("second" . 2))) (lst (make-select-list items 5)))
  (check (select-list-selected-item lst) => '("first" . 1))
  (component-handle-input lst 'down)
  (check (select-list-selected-item lst) => '("second" . 2))
) ;let*

(let ((empty (make-select-list '() 5)))
  (check (select-list-selected-item empty) => #f)
) ;let

(check-report)
