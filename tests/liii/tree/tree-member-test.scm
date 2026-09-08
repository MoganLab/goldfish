(import (liii check)
        (liii tree)
        (scheme char))

(check-set-mode! 'report-failed)

;; tree-member
;; 检查对象是否存在于树结构（嵌套列表）中（默认使用 equal? 比较）。
;; 也可以通过第三个可选参数指定自定义比较谓词。
;;
;; 语法
;; ----
;; (tree-member obj tree)
;; (tree-member obj tree compare)
;;
;; 参数
;; ----
;; obj : any
;; 要搜索的对象。
;;
;; tree : list
;; 要搜索的树结构（嵌套列表）。
;;
;; compare : procedure（可选，默认为 equal?）
;; 用于比较两个对象的二元过程。
;;
;; 返回值
;; ------
;; boolean
;; 如果在树中找到匹配项返回 #t，否则返回 #f。

;; 1. 基本测试与符号测试
(check (tree-member 'a '(a b c)) => #t)
(check (tree-member 'x '(a b c)) => #f)
(check (tree-member 'a '(b (a c))) => #t)
(check (tree-member 'a '((b a) c)) => #t)
(check (tree-member 'a '(((a)))) => #t)
(check (tree-member 'x '(a (b c))) => #f)
(check (tree-member 'a '()) => #f)

;; 2. 字符串内容深度匹配（tree-memq 无法做到的特性）
(check (tree-member "hello" '("hello" "world")) => #t)
(check (tree-member "hello" '("world" ("foo" ("hello" "bar")))) => #t)
(check (tree-member "missing" '("world" ("foo" ("hello" "bar")))) => #f)
(check (tree-member "world" '(1 ("world" 2))) => #t)

;; 3. 子列表结构深度匹配（tree-memq 无法做到的特性）
(check (tree-member '(a b) '(x ((a b) y))) => #t)
(check (tree-member '(1 2) '(0 ((1 2) 3))) => #t)
(check (tree-member '(1 3) '(0 ((1 2) 3))) => #f)
(check (tree-member '(a (b c)) '(x (a (b c)) y)) => #t)

;; 4. 向量深度匹配
(check (tree-member #(1 2 3) '(a (#(1 2 3) b))) => #t)
(check (tree-member #(1 2 4) '(a (#(1 2 3) b))) => #f)

;; 5. 数值（浮点数与负数）深度匹配
(check (tree-member 3.14 '(1 (3.14 2))) => #t)
(check (tree-member 2.718 '(1 (3.14 2))) => #f)
(check (tree-member -42 '(10 (-20 (-42 30)))) => #t)
(check (tree-member -99 '(10 (-20 (-42 30)))) => #f)
(check (tree-member 0 '(1 (0 2))) => #t)
(check (tree-member 0 '(1 (2 3))) => #f)

;; 6. 字符与布尔值
(check (tree-member #\a '(#\b (#\a #\c))) => #t)
(check (tree-member #\z '(#\b (#\a #\c))) => #f)
(check (tree-member #\space '(#\a (#\space #\b))) => #t)
(check (tree-member #t '(1 (#f #t) 2)) => #t)
(check (tree-member #f '(1 (2 3) 4)) => #f)
(check (tree-member #f '(1 (#f) 2)) => #t)

;; 7. 空列表匹配
(check (tree-member '() '(a () b)) => #t)
(check (tree-member '() '(a (b c))) => #t)
(check (tree-member '() '()) => #t)

;; 8. 点对（非真列表 tail）
(check (tree-member 'c '(a b . c)) => #t)
(check (tree-member "tail" '("head" . "tail")) => #t)
(check (tree-member 3 '(1 (2 . 3))) => #t)
(check (tree-member 4 '(1 (2 . 3))) => #f)

;; 9. 对象本身就是整个树
(check (tree-member '(a b c) '(a b c)) => #t)
(check (tree-member '("x" "y") '("x" "y")) => #t)

;; 10. 自定义比较函数 compare
;; 忽略大小写的字符串比较
(check (tree-member "HELLO" '("world" ("hello" "foo")) string-ci=?) => #t)
(check (tree-member "WORLD" '("world" ("hello" "foo")) string-ci=?) => #t)
(check (tree-member "BAR" '("world" ("hello" "foo")) string-ci=?) => #f)

;; 数值跨精度相等比较（如 = 可以认为 1 = 1.0）
(check (tree-member 1 '(0.0 (1.0 2.0)) =) => #t)
(check (tree-member 3 '(0.0 (1.0 2.0)) =) => #f)

;; 自定义二元谓词
(check (tree-member 4 '(1 (3 (5 7))) (lambda (target x) (and (number? x) (even? x)))) => #f)
(check (tree-member 4 '(1 (3 (6 7))) (lambda (target x) (and (number? x) (even? x)))) => #t)

;; 11. 异常测试
(check-catch 'wrong-type-arg (tree-member 'a 123))
(check-catch 'wrong-type-arg (tree-member 'a '(a b) 123))

;; 循环列表检测（开启 safety 时）
(let ((cyclic-tree (list 'a 'b))
      (old-safety (*s7* 'safety)))
  (set-cdr! (cdr cyclic-tree) cyclic-tree)
  (set! (*s7* 'safety) 1)
  (check-catch 'wrong-type-arg (tree-member 'a cyclic-tree))
  (set! (*s7* 'safety) old-safety))

(check-report)
