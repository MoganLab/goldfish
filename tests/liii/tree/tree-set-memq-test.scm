(import (liii check)
        (liii tree)
) ;import

(check-set-mode! 'report-failed)

;; tree-set-memq
;; 检查列表形式的一组符号中是否有任何符号存在于树结构中。
;;
;; 语法
;; ----
;; (tree-set-memq symbols tree)
;;
;; 参数
;; ----
;; symbols : list
;; 要搜索的符号列表（一组符号）。
;;
;; tree : list
;; 要搜索的树结构。
;;
;; 返回值
;; ------
;; boolean
;; 如果 symbols 列表中的任一符号在 tree 中出现，返回 #t，否则返回 #f。
;;
;; 说明
;; ----
;; tree-set-memq 用于高效地检查一组符号中是否有任意成员存在于树结构中。
;; 它是 memq 的批量版本，内部使用符号集合进行快速查找。

;; 基本测试
(check (tree-set-memq '(a b) '(a c d)) => #t)
(check (tree-set-memq '(a b) '(c d e)) => #f)

;; 嵌套列表
(check (tree-set-memq '(a b) '(c (a d))) => #t)
(check (tree-set-memq '(x y) '(a (b c))) => #f)

;; 多个符号都匹配
(check (tree-set-memq '(a b) '((a b c))) => #t)

;; 空列表
(check (tree-set-memq '() '(a b c)) => #f)
(check (tree-set-memq '(a) '()) => #f)

(check-report)
