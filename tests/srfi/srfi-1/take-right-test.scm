;; take-right
;; 从列表尾部提取指定数量的元素。
;;
;; 语法
;; ----
;; (take-right list k)
;;
;; 参数
;; ----
;; list : list?
;; 源列表，从中从尾部提取元素。
;;
;; k : integer?
;; 要从尾部提取的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 包含从尾部开始指定数量元素的新列表。
;;
;; 错误处理
;; --------
;; - out-of-range：当 k 超过列表长度或 k 为负数时抛出。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (take-right '(1 2 3 4) 3) => '(2 3 4))
(check (take-right '(1 2 3 4) 4) => '(1 2 3 4))
(check (take-right '(1 2 3 . 4) 3) => '(1 2 3 . 4))

(check (take-right '() 0) => '())
(check (take-right '(a) 1) => '(a))
(check (take-right '(a) 0) => '())
(check (take-right '((a) (b c) d) 2) => '((b c) d))
(check (take-right '(1 2 3 4 5 6) 3) => '(4 5 6))

(check (take-right (drop '(1 2 3 4 5) 1) 3) => '(3 4 5))
(check (take-right (take '(1 2 3 4 5) 4) 2) => '(3 4))

(check (take-right (iota 10) 5) => '(5 6 7 8 9))

(check (take-right '(1) 1) => '(1))
(check (take-right '(1 2) 1) => '(2))

(check-catch 'out-of-range (take-right '(1 2 3 4) 5))
(check-catch 'out-of-range (take-right '(1 2 3 . 4) 4))
(check-catch 'out-of-range (take-right '(1 2 3) -1))
(check-catch 'type-error (take-right "not a list" 2))
(check-catch 'type-error (take-right '(1 2 3) "not a number"))

;; take-right 与原列表共享节点：返回的是原列表的子列表
(let ((l (list 1 2 3 4 5)))
  (check (eq? (take-right l 2) (cdddr l)) => #t)
  (check (eq? (take-right l 5) l) => #t)
  (check (eq? (take-right l 0) '()) => #t))

;; 大列表 + GC 压力回归测试
(let ((l (iota 10000)))
  (check (take-right l 5000) => (iota 5000 5000))
  (check (length (take-right l 10000)) => 10000)
  (check l => (iota 10000)))

(do ((i 0 (+ i 1)))
  ((= i 1000))
  (take-right (iota 100) 50))

(check-report)
