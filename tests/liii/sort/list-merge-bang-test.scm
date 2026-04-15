(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-merge!
;; 原地合并两个已排序的列表。
;;
;; 语法
;; ----
;; (list-merge! cmp lst1 lst2)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; lst1 : list
;; 第一个已排序列表。
;;
;; lst2 : list
;; 第二个已排序列表。
;;
;; 返回值
;; ----
;; list
;; 合并后的列表。
;;
;; 注意
;; ----
;; 这是一个破坏性操作，可能会修改输入列表。
;;
;; 示例
;; ----
;; (list-merge! < '(1 3 5) '(2 4 6)) => '(1 2 3 4 5 6)
;;
;; 错误处理
;; ----
;; 无


;; 基本合并测试
(define lis1 '(1 3 5))
(define lis2 '(2 4 6))
(check (list-merge! < lis1 lis2)
  =>
  '(1 2 3 4 5 6)
) ;check


;; 包含重复元素的合并
(define lis3 '(1 1 3))
(define lis4 '(1 2 4))
(check (list-merge! < lis3 lis4)
  =>
  '(1 1 1 2 3 4)
) ;check


;; 包含空列表的合并
(define lis5 '())
(define lis6 '(1 2 3))
(check (list-merge! < lis5 lis6)
  =>
  '(1 2 3)
) ;check
(check (list-merge! < lis6 lis5)
  =>
  '(1 2 3)
) ;check


;; 两个空列表
(define lis7 '())
(define lis8 '())
(check (list-merge! < lis7 lis8) => '())


(check-report)
