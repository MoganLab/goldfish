(import (liii check) (liii tree))

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

;; 字符
(check (tree-memq #\a '(#\b (#\a #\c))) => #t)
(check (tree-memq #\z '(#\b (#\a #\c))) => #f)
(check (tree-memq #\space '(#\a (#\b (#\space #\c)))) => #t)
(check (tree-memq #\newline '(#\a (#\b #\c))) => #f)

;; 布尔值
(check (tree-memq #t '(1 (#f #t) 2)) => #t)
(check (tree-memq #f '(1 (2 3) 4)) => #f)
(check (tree-memq #f '(1 (#f) 2)) => #t)
(check (tree-memq #t '((#f) (((#f))))) => #f)

;; 整数（小整数由 s7 缓存可直接 eq?；负数或大整数作为同一引用比较）
(check (tree-memq 1 '(2 (3 (1 4)))) => #t)
(check (tree-memq 0 '(1 (2 3))) => #f)
(check (tree-memq 0 '(1 (0 2))) => #t)
(check (tree-memq 10 '(1 (2 (10 3)))) => #t)
(let ((n -42))
  (check (tree-memq n (list 10 (list -20 (list n 30)))) => #t)
  (check (tree-memq n (list 10 (list -20 30))) => #f))

;; 过程 (procedure)
(check (tree-memq + (list 1 (list - +) 2)) => #t)
(check (tree-memq * (list 1 (list - +) 2)) => #f)

;; 引用相同的复合对象（字符串、pair、vector）
(let ((s "hello")
      (s2 "world"))
  (check (tree-memq s (list 1 (list s 2))) => #t)
  (check (tree-memq s2 (list 1 (list s 2))) => #f))

(let ((p (cons 1 2)))
  (check (tree-memq p (list 'a (list p 'b))) => #t)
  (check (tree-memq (cons 1 2) (list 'a (list p 'b))) => #f))

(let ((v (vector 1 2 3)))
  (check (tree-memq v (list 0 (list 1 v 2) 3)) => #t)
  (check (tree-memq (vector 1 2 3) (list 0 (list 1 v 2) 3)) => #f))

;; 空列表作为树内元素（因为所有真列表末尾均为 ()，故在任何真列表中查找 () 均为 #t）
(check (tree-memq '() '(a () b)) => #t)
(check (tree-memq '() '(a (b c))) => #t)
(check (tree-memq '() '()) => #t)

;; 点对（非真列表 tail）
(check (tree-memq 'c '(a b . c)) => #t)
(check (tree-memq 3 '(1 (2 . 3))) => #t)
(check (tree-memq 4 '(1 (2 . 3))) => #f)

;; obj 本身就是整个树对象
(let ((t '(a (b c))))
  (check (tree-memq t t) => #t))

;; 异常测试：非列表树结构
(check-catch 'wrong-type-arg (tree-memq 'a 123))

;; 异常测试：开启 safety 时的循环列表检测
(let ((cyclic-tree (list 'a 'b))
      (old-safety (*s7* 'safety)))
  (set-cdr! (cdr cyclic-tree) cyclic-tree)
  (set! (*s7* 'safety) 1)
  (check-catch 'wrong-type-arg (tree-memq 'a cyclic-tree))
  (set! (*s7* 'safety) old-safety))



(check-report)
