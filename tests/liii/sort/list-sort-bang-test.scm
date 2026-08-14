(import (liii check) (liii sort) (liii list))

(check-set-mode! 'report-failed)

;; list-sort!
;; 对列表进行原地排序（破坏性操作），返回排序后的列表本身。
;;
;; 语法
;; ----
;; (list-sort! less? lst)
;;
;; 参数
;; ----
;; less? : procedure
;; 比较函数，接受两个元素，当第一个元素应排在前面时返回 #t。
;;
;; lst : list
;; 要排序的列表。
;;
;; 返回值
;; ----
;; list
;; 排序后的列表，与输入 lst 是同一个对象（eq? 为 #t）。
;;
;; 说明
;; ----
;; 1. 本实现基于 S7 内置的 sort!：只重写各 pair 的 car，
;;    列表骨架（cdr 链）保持不变，因此所有指向该列表的别名
;;    都能观察到排序结果
;; 2. 不稳定排序：比较结果相等的元素，排序后的相对顺序不保证
;;    与原来一致；需要稳定排序时请使用 list-stable-sort!
;; 3. 与内置 sort! 的参数顺序不同：
;;    list-sort! 是 (list-sort! less? lst)，比较函数在前；
;;    内置 sort! 是 (sort! seq less?)，序列在前
;;
;; 示例
;; ----
;; (list-sort! < (list 1 5 1 0 -1 9 2 4 3)) => '(-1 0 1 1 2 3 4 5 9)
;;
;; 错误处理
;; ----
;; 当 lst 不是列表时抛出 wrong-type-arg 错误。

;; 基本升序排序：精确内容断言
(check (list-sort! < (list 3 1 4 1 5 9 2 6 5)) => '(1 1 2 3 4 5 5 6 9))
(check (list-sort! < (list 1 5 1 0 -1 9 2 4 3)) => '(-1 0 1 1 2 3 4 5 9))

;; 降序排序
(check (list-sort! > (list 1 5 1 0 -1 9 2 4 3)) => '(9 5 4 3 2 1 1 0 -1))

;; 边界情况：空列表与单元素
(check (list-sort! < '()) => '())
(check (list-sort! < (list 42)) => '(42))

;; 已排序与逆序输入
(check (list-sort! < (list 1 2 3 4 5)) => '(1 2 3 4 5))
(check (list-sort! < (list 5 4 3 2 1)) => '(1 2 3 4 5))

;; 重复元素与全相同元素
(check (list-sort! < (list 3 1 4 1 5 9 2 6 5 3 5)) => '(1 1 2 3 3 4 5 5 5 6 9))
(check (list-sort! < (list 7 7 7 7)) => '(7 7 7 7))

;; 含负数
(check (list-sort! < (list 0 -1 2 -2 3 1)) => '(-2 -1 0 1 2 3))
(check (list-sort! < (list 5 -3 0 2 1 -1 4)) => '(-3 -1 0 1 2 4 5))

;; 字符串列表
(check (list-sort! string<? (list "pear" "apple" "banana"))
  =>
  '("apple" "banana" "pear")
) ;check

;; 自定义比较函数：按字符串长度排序
(check (list-sort! (lambda (x y) (< (string-length x) (string-length y)))
         (list "ccc" "a" "bb")
       ) ;list-sort!
  =>
  '("a" "bb" "ccc")
) ;check

;; 返回值与输入是同一个对象
(let ((xs (list 3 1 2)))
  (check-true (eq? xs (list-sort! < xs)))
  (check xs => '(1 2 3))
) ;let

;; 列表骨架不变：别名可见排序结果
(let ((xs (list 3 1 2)))
  (let ((alias xs))
    (list-sort! < xs)
    (check alias => '(1 2 3))
  ) ;let
) ;let

;; 较大列表：验证排序正确性
(check-true (list-sorted? < (list-sort! < (reverse (iota 1000)))))
(check (list-sort! < (reverse (iota 10))) => (iota 10))

;; 错误处理：非列表参数
(check-catch 'wrong-type-arg (list-sort! < 42))

(check-report)
