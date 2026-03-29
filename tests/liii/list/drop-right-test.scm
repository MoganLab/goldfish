(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; drop-right 函数测试
;;
;; 从列表末尾删除指定数量的元素。
;;
;; 语法
;; ----
;; (drop-right list k)
;;
;; 参数
;; ----
;; list : list?
;; 源列表，从中从末尾删除元素。
;;
;; k : integer?
;; 要从末尾删除的元素数量，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; list
;; 删除k个尾部元素后的新列表。当k等于列表长度时，返回空列表。
;;
;; 说明
;; ----
;; drop-right函数与drop功能相反，从列表右侧（尾部）删除元素而不是前端。
;; 对于proper list，返回的是列表前端部分；
;; 对于dotted list，如果k等于列表长度减一，返回的是空列表'()。
;;
;; drop-right和take-right互为补操作：(append (drop-right lst k) (take-right lst k)) = lst
;;
;; 示例
;; ----
;; (drop-right '(1 2 3 4) 2) => '(1 2)
;; (drop-right '(a b c) 1) => '(a b)
;; (drop-right '(a b c) 0) => '(a b c)
;; (drop-right '(1 (2 3) 4) 2) => '(1 (2 3))
;;
;; 与dotted list交互:
;; (drop-right '(a b c . d) 1) => '(a b)
;; (drop-right '(a b . d) 2) => '()
;;
;; 边界条件
;; --------
;; - 空列表：返回空列表
;; - 零个元素删除：返回原列表
;; - 删除所有元素：返回空列表
;;
;; 错误处理
;; --------
;; - out-of-range：当k超过列表长度时
;; - wrong-type-arg：当list不是列表或k不是整数类型时

(check (drop-right '(1 2 3 4) 2) => '(1 2))
(check (drop-right '(1 2 3 4) 4) => '())
(check (drop-right '(1 2 3 . 4) 3) => '())

; 基本功能测试
(check (drop-right '(1 2 3 4 5) 0) => '(1 2 3 4 5))
(check (drop-right '(1 2 3 4 5) 1) => '(1 2 3 4))
(check (drop-right '(1 2 3 4 5) 3) => '(1 2))
(check (drop-right '(1 2 3 4 5) 5) => '())

; 空列表边界条件
(check (drop-right '() 0) => '())

; 单元素列表测试
(check (drop-right '(a) 0) => '(a))
(check (drop-right '(a) 1) => '())

; 嵌套列表测试
(check (drop-right '((a b) (c d) (e f)) 1) => '((a b) (c d)))
(check (drop-right '((a b) (c d) (e f)) 2) => '((a b)))
(check (drop-right '((a b) (c d) (e f)) 3) => '())

; dotted list边界条件测试
(check (drop-right '(1 2 . 3) 0) => '(1 2))
(check (drop-right '(1 2 . 3) 1) => '(1))
(check (drop-right '(1 2 . 3) 2) => '())
(check (drop-right '(a b c . d) 1) => '(a b))
(check (drop-right '(a b c . d) 2) => '(a))
(check (drop-right '(a b c . d) 3) => '())

; 链式操作测试
(check (drop-right (drop-right '(1 2 3 4 5) 1) 1) => '(1 2 3))
(check (drop-right (take-right '(1 2 3 4 5) 4) 2) => '(2 3))
(check (take-right (drop-right '(1 2 3 4 5) 2) 2) => '(2 3))

; 大列表测试
(check (drop-right (iota 10) 5) => '(0 1 2 3 4))
(check (drop-right (iota 10) 0) => '(0 1 2 3 4 5 6 7 8 9))
(check (drop-right (iota 10) 10) => '())

; 与take-right的对称性测试
(let ((lst '(1 2 3 4 5 6 7 8 9 10)))
  (define (symmetry-test lst k)
    (append (drop-right lst k) (take-right lst k))
  ) ;define

  (check (symmetry-test lst 0) => lst)
  (check (symmetry-test lst 3) => lst)
  (check (symmetry-test lst 10) => lst)
  (check (symmetry-test lst 5) => lst)
) ;let

; 不同数据类型测试
(check (drop-right '("a" "b" "c" "d") 2) => '("a" "b"))
(check (drop-right '(42 43 44 45) 2) => '(42 43))
(check (drop-right '(#t #f #t #f) 2) => '(#t #f))
(check (drop-right '(a 1 "hello") 1) => '(a 1))

; 错误条件测试
(check-catch 'out-of-range (drop-right '(1 2 3 4) 5))
(check-catch 'out-of-range (drop-right '(1 2 3 4) -1))
(check-catch 'out-of-range (drop-right '(1 2 3 . 4) 4))
(check-catch 'wrong-type-arg (drop-right "not a list" 2))
(check-catch 'wrong-type-arg (drop-right '(1 2 3) "not a number"))

(check-report)
