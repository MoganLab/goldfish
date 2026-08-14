(import (liii check) (liii sort) (liii list))

(check-set-mode! 'report-failed)

;; vector-sort!
;; 对向量进行原地排序（破坏性操作），返回排序后的向量本身。
;;
;; 语法
;; ----
;; (vector-sort! less? vec)
;; (vector-sort! less? vec start)
;; (vector-sort! less? vec start end)
;;
;; 参数
;; ----
;; less? : procedure
;; 比较函数，接受两个元素，当第一个元素应排在前面时返回 #t。
;;
;; vec : vector
;; 要排序的向量。
;;
;; start : integer
;; 排序区间的起始下标（包含），默认为 0。
;;
;; end : integer
;; 排序区间的结束下标（不包含），默认为 (vector-length vec)。
;;
;; 返回值
;; ----
;; vector
;; 排序后的向量，与输入 vec 是同一个对象（eq? 为 #t）。
;;
;; 说明
;; ----
;; 1. 本实现基于 S7 内置的 sort!：基于 C qsort 的原地排序，
;;    直接重写向量元素，因此所有指向该向量的别名
;;    都能观察到排序结果
;; 2. 不稳定排序：比较结果相等的元素，排序后的相对顺序不保证
;;    与原来一致；需要稳定排序时请使用 vector-stable-sort!
;; 3. 与内置 sort! 的参数顺序不同：
;;    vector-sort! 是 (vector-sort! less? vec)，比较函数在前；
;;    内置 sort! 是 (sort! seq less?)，序列在前
;; 4. 指定 start/end 时只排序 [start, end) 区间，
;;    区间之外的元素保持不变
;;
;; 示例
;; ----
;; (vector-sort! < #(3 1 4 1 5 9 2 6 5)) => #(1 1 2 3 4 5 5 6 9)
;; (vector-sort! < #(9 3 1 2 8) 1 4)     => #(9 1 2 3 8)
;;
;; 错误处理
;; ----
;; 当 vec 不是向量时抛出 type-error 错误；
;; 当 start/end 越界或 start > end 时抛出 value-error 错误。

;; 基本升序排序：精确内容断言
(check (vector-sort! < (vector 3 1 4 1 5 9 2 6 5)) => #(1 1 2 3 4 5 5 6 9))
(check (vector-sort! < (vector 1 5 1 0 -1 9 2 4 3)) => #(-1 0 1 1 2 3 4 5 9))

;; 降序排序
(check (vector-sort! > (vector 1 5 1 0 -1 9 2 4 3)) => #(9 5 4 3 2 1 1 0 -1))

;; 边界情况：空向量与单元素
(check (vector-sort! < #()) => #())
(check (vector-sort! < (vector 42)) => #(42))

;; 已排序与逆序输入
(check (vector-sort! < (vector 1 2 3 4 5)) => #(1 2 3 4 5))
(check (vector-sort! < (vector 5 4 3 2 1)) => #(1 2 3 4 5))

;; 重复元素与全相同元素
(check (vector-sort! < (vector 3 1 4 1 5 9 2 6 5 3 5))
  =>
  #(1 1 2 3 3 4 5 5 5 6 9)
) ;check
(check (vector-sort! < (vector 7 7 7 7)) => #(7 7 7 7))

;; 含负数
(check (vector-sort! < (vector 0 -1 2 -2 3 1)) => #(-2 -1 0 1 2 3))
(check (vector-sort! < (vector 5 -3 0 2 1 -1 4)) => #(-3 -1 0 1 2 4 5))

;; 字符串向量
(check (vector-sort! string<? (vector "pear" "apple" "banana"))
  =>
  #("apple" "banana" "pear")
) ;check

;; 自定义比较函数：按字符串长度排序
(check (vector-sort! (lambda (x y) (< (string-length x) (string-length y)))
         (vector "ccc" "a" "bb")
       ) ;vector-sort!
  =>
  #("a" "bb" "ccc")
) ;check

;; 返回值与输入是同一个对象
(let ((v (vector 3 1 2)))
  (check-true (eq? v (vector-sort! < v)))
  (check v => #(1 2 3))
) ;let

;; 原地排序：别名可见排序结果
(let ((v (vector 3 1 2)))
  (let ((alias v))
    (vector-sort! < v)
    (check alias => #(1 2 3))
  ) ;let
) ;let

;; 指定 start：只排序 [start, 末尾) 区间
(check (vector-sort! < (vector 9 3 1 2) 1) => #(9 1 2 3))

;; 指定 start 和 end：只排序 [start, end) 区间，区间外元素不变
(check (vector-sort! < (vector 9 3 1 2 8) 1 4) => #(9 1 2 3 8))
(check (vector-sort! > (vector 0 1 5 3 9 0) 1 5) => #(0 9 5 3 1 0))

;; 区间为空（start = end）时向量不变
(check (vector-sort! < (vector 2 1) 1 1) => #(2 1))
(check (vector-sort! < (vector 2 1) 2 2) => #(2 1))

;; 区间排序同样返回原向量
(let ((v (vector 9 3 1 2 8)))
  (check-true (eq? v (vector-sort! < v 1 4)))
  (check v => #(9 1 2 3 8))
) ;let

;; 较大向量：验证排序正确性
(check (vector-sort! < (list->vector (reverse (iota 10))))
  =>
  (list->vector (iota 10))
) ;check
(check-true (vector-sorted? < (vector-sort! < (list->vector (reverse (iota 1000)))))
) ;check-true

;; 错误处理：非向量参数
(check-catch 'type-error (vector-sort! < 42))
(check-catch 'type-error (vector-sort! < '(3 1 2)))
(check-catch 'type-error (vector-sort! < '(3 1 2) 0))
(check-catch 'type-error (vector-sort! < "cba" 0 3))

;; 错误处理：start/end 越界或 start > end
(check-catch 'value-error (vector-sort! < (vector 2 1) -1))
(check-catch 'value-error (vector-sort! < (vector 2 1) 0 3))
(check-catch 'value-error (vector-sort! < (vector 2 1) 2 1))

(check-report)
