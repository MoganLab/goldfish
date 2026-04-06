(import (liii check)
        (liii tree)
) ;import

(check-set-mode! 'report-failed)

;; tree-count
;; 统计对象在树结构（嵌套列表）中出现的次数。
;;
;; 语法
;; ----
;; (tree-count obj tree)
;; (tree-count obj tree max-count)
;;
;; 参数
;; ----
;; obj : any
;; 要搜索的对象（使用 eq? 进行比较）。
;;
;; tree : list
;; 要搜索的树结构（嵌套列表）。
;;
;; max-count : integer（可选）
;; 最大计数限制。当计数达到此值时停止搜索并返回。
;;
;; 返回值
;; ------
;; integer
;; 对象在树中出现的次数。
;;
;; 说明
;; ----
;; tree-count 递归地遍历树结构，使用 eq? 比较对象。
;; 它会深入搜索所有嵌套的子列表。

;; 基本功能测试
(check (tree-count 'a '(a b a c a d)) => 3)
(check (tree-count 'x '(a b c)) => 0)

;; 嵌套列表测试
(check (tree-count 'a '(a (b a) (c (a d)))) => 3)
(check (tree-count 'b '((a b) (c (b d)))) => 2)

;; max-count 参数测试
(check (tree-count 'a '(a b a c a d a e) 3) => 3)
(check (tree-count 'a '(a b a) 5) => 2)
(check (tree-count 'a '(a b a c) 1) => 1)

;; 空列表测试
(check (tree-count 'a '()) => 0)

;; 复杂嵌套测试
(check (tree-count 1 '(1 (2 1) (3 (4 1)))) => 3)
;; 空列表测试：'(() a (b ())) 中有 4 个空列表：
;; 1. 第一个元素 ()
;; 2. (b ()) 中的 ()
;; 3. 整个列表的结尾 ()
;; 4. (b ()) 子列表的结尾 ()
(check (tree-count '() '(() a (b ()))) => 4)

(check-report)
