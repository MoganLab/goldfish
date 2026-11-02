;; take
;; 从列表开头提取指定数量的元素。
;;
;; 语法
;; ----
;; (take list k)
;;
;; 参数
;; ----
;; list : list?
;; 源列表，从中提取元素。
;;
;; k : integer?
;; 要提取的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 包含指定数量元素的新列表，从原列表的开头开始计数。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (take '(1 2 3 4) 3) => '(1 2 3))
(check (take '(1 2 3 4) 4) => '(1 2 3 4))
(check (take '(1 2 3 . 4) 3) => '(1 2 3))

(check-catch 'type-error (take '(1 2 3 4) 5))
(check-catch 'type-error (take '(1 2 3 . 4) 4))

(check (take '() 0) => '())
(check (take '(a) 1) => '(a))
(check (take '(a) 0) => '())
(check (take '((a) (b c) d) 2) => '((a) (b c)))
(check (take '(1 2 3 4 5 6) 3) => '(1 2 3))

(check (take (drop '(1 2 3 4 5) 1) 3) => '(2 3 4))
(check (take (take '(1 2 3 4 5) 4) 2) => '(1 2))

(check (take (iota 10) 5) => '(0 1 2 3 4))

(check-catch 'out-of-range (take '(1 2 3) -1))
(check-catch 'type-error (take "not a list" 2))
(check-catch 'type-error (take '(1 2 3) "not a number"))

;; take 总是创建新的列表结构，即使取走全部元素也不共享节点
(let ((l (list 1 2 3)))
  (check (eq? (take l 3) l) => #f)
  (check (take l 3) => '(1 2 3))
  ;; 修改结果不影响原列表
  (let ((r (take l 2)))
    (set-car! r 99)
    (check l => '(1 2 3))))

;; 大列表 + GC 压力回归测试
(let ((l (iota 10000)))
  (check (take l 5000) => (iota 5000))
  (check (length (take l 10000)) => 10000)
  (check l => (iota 10000)))

(do ((i 0 (+ i 1)))
  ((= i 1000))
  (take (iota 100) 50))

(check-report)
