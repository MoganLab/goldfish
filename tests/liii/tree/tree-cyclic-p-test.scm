(import (liii check)
        (liii tree)
) ;import

(check-set-mode! 'report-failed)

;; tree-cyclic?
;; 检查树结构是否有循环。
;;
;; 语法
;; ----
;; (tree-cyclic? tree)
;;
;; 参数
;; ----
;; tree : list
;; 要检查的树结构。
;;
;; 返回值
;; ------
;; boolean
;; 如果树有循环返回 #t，否则返回 #f。

;; 非循环树
(check (tree-cyclic? '(a b c)) => #f)
(check (tree-cyclic? '(a (b c))) => #f)
(check (tree-cyclic? '()) => #f)

;; 循环树测试
;; 创建一个循环结构: (a b . #0#) 其中 #0# 指向列表本身
(let ((cyclic-list (list 'a 'b)))
  (set-cdr! (cdr cyclic-list) cyclic-list)
  (check (tree-cyclic? cyclic-list) => #t)
) ;let

;; 嵌套循环结构
(let ((inner (list 'a 'b)))
  (set-cdr! (cdr inner) inner)
  (check (tree-cyclic? (list 'x inner)) => #t)
) ;let

(check-report)
