(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; select-first-line-children
;; 选择 env 首行可内联摆放的子节点对列表（子节点 . 目标列）。
;;
;; 语法
;; ----
;; (select-first-line-children node first-column)
;;
;; 返回值
;; ------
;; pair? 列表，元素为 (child-node . column)。

;; 测试：全部子节点可内联时全部选中
(let* ((node (scan '(+ 1 2)))
       (selected (select-first-line-children node 1)))
  (check (>= (selected-count selected) 1) => #t)
) ;let*

;; 测试：无法内联的子节点终止选择
(let* ((node (scan '(list 1 (a (b (c (d (e (f (g (h (i (j)))))))))))))
       (selected (select-first-line-children node 6)))
  (check (> (selected-count selected) 0) => #t)
  (check (<= (selected-count selected) 2) => #t)
) ;let*

;; 测试：selected-child / selected-column 访问选中项
(let* ((node (scan '(+ 1 2)))
       (selected (select-first-line-children node 1)))
  (check (atom? (selected-child (car selected))) => #t)
  (check (number? (selected-column (car selected))) => #t)
) ;let*

(check-report)
