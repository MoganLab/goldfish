;; selected-child
;; 获取选中项中的子节点。
;;
;; 语法
;; ----
;; (selected-child pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 由 `select-first-line-children` 返回的选中项。
;;
;; 返回值
;; ----
;; any?
;; 选中的子节点，通常是 env? 或 atom?。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

(let* ((node (scan '(+ 1 2))) (selected (select-first-line-children node 1)))
  (check (atom? (selected-child (car selected))) => #t)
  (check (atom-value (selected-child (car selected))) => 1)
) ;let*

(check-report)
