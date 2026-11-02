;; select-list-set-selected-index!
;; 设置选择列表组件当前的选中条目索引。
;;
;; 语法
;; ----
;; (select-list-set-selected-index! comp index)
;;
;; 参数
;; ----
;; comp : select-list?
;; 选择列表组件实例。
;;
;; index : integer?
;; 目标选中索引。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui select-list))

(check-set-mode! 'report-failed)

(let* ((items '((a) (b) (c))) (lst (make-select-list items 5)))
  (check (select-list-selected-index lst) => 0)
  (select-list-set-selected-index! lst 2)
  (check (select-list-selected-index lst) => 2)
) ;let*

(check-report)
