;; selected-column
;; 获取选中项的目标列号。
;;
;; 语法
;; ----
;; (selected-column pair)
;;
;; 参数
;; ----
;; pair : pair?
;; 由 `select-first-line-children` 返回的选中项。
;;
;; 返回值
;; ----
;; integer?
;; 选中项在首行中的目标列号。

(import (liii check)
  (liii goldfmt format writer)
  (liii goldfmt record)
  (liii goldfmt scan)
) ;import

(check-set-mode! 'report-failed)

(let* ((node (scan '(+ 1 2))) (selected (select-first-line-children node 1)))
  (check (number? (selected-column (car selected))) => #t)
  (check (selected-column (car selected)) => 2)
) ;let*

(check-report)
