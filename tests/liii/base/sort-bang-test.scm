(import (liii check) (liii base))

(check-set-mode! 'report-failed)

;; sort!
;; 对序列进行原地排序，返回排序后的序列本身（S7 内置函数）。
;;
;; 语法
;; ----
;; (sort! seq less?)
;;
;; 参数
;; ----
;; seq : list? 或 vector? 或 string?
;;     要排序的序列。
;;
;; less? : procedure
;;     比较函数，接受两个元素，当第一个元素应排在前面时返回 #t。
;;
;; 返回值
;; ------
;; list? 或 vector? 或 string?
;;     排序后的序列，与输入 seq 是同一个对象（eq? 为 #t）。
;;
;; 说明
;; ----
;; 1. 原地排序：对列表只重写各 pair 的 car，列表骨架（cdr 链）保持不变，
;;    因此所有指向该列表的别名都能观察到排序结果
;; 2. 不稳定排序：比较结果相等的元素，排序后的相对顺序不保证与原来一致；
;;    需要稳定排序时请使用 (liii sort) 的 list-stable-sort! 或 vector-stable-sort!
;; 3. 支持列表、向量和字符串；字符串按字符排序，比较函数常用 char<?
;; 4. 与 (liii sort) 的 list-sort! 参数顺序不同：
;;    内置 sort! 是 (sort! seq less?)，序列在前；
;;    SRFI-132 的 list-sort! 是 (list-sort! less? lst)，比较函数在前
;; 5. 比较函数必须一致（不能对相同的两个元素返回矛盾的结果），
;;    否则可能触发死循环检测而报错
;;
;; 与 list-sort! 的区别
;; -------------------
;; (liii sort) 的 list-sort! 是基于 sort! 的薄封装，两者原地排序的
;; 语义一致（只改 car 不动列表骨架，返回值与输入 eq?），区别在于：
;; sort!       支持列表、向量和字符串，参数顺序是 (sort! seq less?)；
;; list-sort!  只支持列表，参数顺序是 (list-sort! less? lst)，
;;             比较函数在前（SRFI-132 约定）。
;;
;; 相关函数
;; --------
;; list-sort!        - (liii sort) 中的列表排序，比较函数在前
;; list-stable-sort! - (liii sort) 中的稳定列表排序
;; vector-sort!      - (liii sort) 中的向量排序
;;
;; 错误处理
;; --------
;; 当 seq 不是序列时抛出 wrong-type-arg 错误；
;; 当 less? 不是可接受两个参数的函数时抛出错误。

;; 列表排序：升序与降序
(check (sort! (list 3 1 2) <) => '(1 2 3))
(check (sort! (list 3 1 4 1 5 9 2 6) <) => '(1 1 2 3 4 5 6 9))
(check (sort! (list 1 2 3) >) => '(3 2 1))

;; 边界情况
(check (sort! '() <) => '())
(check (sort! (list 42) <) => '(42))

;; 返回值与输入是同一个对象
(let ((xs (list 3 1 2)))
  (check-true (eq? xs (sort! xs <)))
  (check xs => '(1 2 3))
) ;let

;; 列表骨架不变：别名可见排序结果
(let ((xs (list 3 1 2)))
  (let ((alias xs))
    (sort! xs <)
    (check alias => '(1 2 3))
  ) ;let
) ;let

;; 向量排序
(check (sort! (vector 3 1 2) <) => #(1 2 3))
(check (sort! (vector 5 4 3 2 1) >) => #(5 4 3 2 1))
(let ((v (vector 3 1 2)))
  (check-true (eq? v (sort! v <)))
  (check v => #(1 2 3))
) ;let

;; 字符串排序（按字符）
(check (sort! (string #\c #\b #\a) char<?) => "abc")
(check (sort! (string #\b #\a #\c) char>?) => "cba")

;; 自定义比较函数：按字符串长度排序
(check (sort! (list "ccc" "a" "bb")
         (lambda (x y) (< (string-length x) (string-length y)))
       ) ;sort!
  =>
  '("a" "bb" "ccc")
) ;check

;; 错误处理：非序列参数
(check-catch 'wrong-type-arg (sort! 42 <))
(check-catch 'wrong-type-arg (sort! #t <))

(check-report)
