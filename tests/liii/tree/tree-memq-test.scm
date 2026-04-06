(import (liii check)
        (liii tree)
) ;import

(check-set-mode! 'report-failed)

;; tree-memq
;; 检查对象是否存在于树结构中（使用 eq? 比较）。
;; 这是树版本的 memq，如果对象在树中返回 #t。
;;
;; 语法
;; ----
;; (tree-memq obj tree)
;;
;; 参数
;; ----
;; obj : any
;; 要搜索的对象。
;;
;; tree : list
;; 要搜索的树结构。
;;
;; 返回值
;; ------
;; boolean
;; 如果对象在树中返回 #t，否则返回 #f。

;; 基本测试
(check (tree-memq 'a '(a b c)) => #t)
(check (tree-memq 'x '(a b c)) => #f)

;; 嵌套列表
(check (tree-memq 'a '(b (a c))) => #t)
(check (tree-memq 'a '((b a) c)) => #t)
(check (tree-memq 'a '(((a)))) => #t)
(check (tree-memq 'x '(a (b c))) => #f)

;; 空列表
(check (tree-memq 'a '()) => #f)

(check-report)
