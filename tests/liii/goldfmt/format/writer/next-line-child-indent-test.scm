(import (liii check))
(import (liii goldfmt format writer) (liii goldfmt record) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; next-line-child-indent
;; 计算首行选中项之后子节点的缩进列。
;;
;; 语法
;; ----
;; (next-line-child-indent parent parent-indent selected rest-start)
;;
;; 返回值
;; ------
;; integer?

;; 测试：匿名子 env 时缩进为父缩进加一
(let* ((node (scan '((lambda (x) x) 1)))
       (selected (select-first-line-children node 1))
       (indent (next-line-child-indent node 0 selected (selected-count selected))))
  (check (= indent 1) => #t)
) ;let*

;; 测试：默认策略下缩进为父缩进加二
(let* ((node (scan '(verylongtagname a b c d e f g h i j k l)))
       (selected (select-first-line-children node 16))
       (indent (next-line-child-indent node 0 selected (selected-count selected))))
  (check (= indent 2) => #t)
) ;let*

(check-report)
