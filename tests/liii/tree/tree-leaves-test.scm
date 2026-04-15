(import (liii check) (liii tree))

(check-set-mode! 'report-failed)

;; tree-leaves
;; 返回树结构中的叶子节点数量。
;;
;; 语法
;; ----
;; (tree-leaves tree)
;;
;; 参数
;; ----
;; tree : list
;; 要计算叶子数量的树结构。
;;
;; 返回值
;; ------
;; integer
;; 树的叶子节点数。

;; 简单列表
(check (tree-leaves '(a b c)) => 3)
(check (tree-leaves '()) => 0)

;; 嵌套列表
(check (tree-leaves '(a (b c))) => 3)
(check (tree-leaves '((a b) (c d)))
  =>
  4
) ;check
(check (tree-leaves '(a (b (c d))))
  =>
  4
) ;check

;; 更深嵌套
(check (tree-leaves '(((a)))) => 1)
(check (tree-leaves '((a b) ((c d) e)))
  =>
  5
) ;check

(check-report)
