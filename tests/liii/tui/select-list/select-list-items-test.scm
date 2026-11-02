;; select-list-items
;; 获取选择列表组件当前的候选条目列表。
;;
;; 语法
;; ----
;; (select-list-items comp)
;;
;; 参数
;; ----
;; comp : select-list?
;; 选择列表组件实例。
;;
;; 返回值
;; ----
;; list?
;; 候选条目列表。

(import (scheme base) (liii check) (liii tui select-list))

(check-set-mode! 'report-failed)

(let* ((items '(("a" . 1) ("b" . 2))) (lst (make-select-list items 5)))
  (check (select-list-items lst) => items)
) ;let*

(check-report)
