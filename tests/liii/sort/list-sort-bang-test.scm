(import (liii check)
        (liii sort)
) ;import

(check-set-mode! 'report-failed)

;; list-sort!
;; 对列表进行破坏性快速排序。
;;
;; 语法
;; ----
;; (list-sort! cmp lst)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; lst : list
;; 要排序的列表。
;;
;; 返回值
;; ----
;; list
;; 排序后的列表（原地修改）。
;;
;; 注意
;; ----
;; 这是一个破坏性操作，原列表会被修改。
;;
;; 示例
;; ----
;; (list-sort! < '(1 5 1 0 -1 9 2 4 3)) => 排序后的列表
;;
;; 错误处理
;; ----
;; 无

(check-true (list-sorted? < (list-sort! < '(1 5 1 0 -1 9 2 4 3))))
(check-true (list-sorted? < (list-sort! < '(9 7 5 3 2 8 6 4 1))))
(check-true (list-sorted? < (list-sort! < '())))
(check-true (list-sorted? < (list-sort! < '(42))))
(check-true (list-sorted? < (list-sort! < '(1 2 3 4 5))))
(check-true (list-sorted? < (list-sort! < '(3 1 4 1 5 9 2 6 5 3 5))))
(check-true (list-sorted? < (list-sort! < '(0 -1 2 -2 3 1))))
(check-true (list-sorted? < (list-sort! < '(5 -3 0 2 1 -1 4))))

;; 降序排序
(check-true (list-sorted? > (list-sort! > '(1 5 1 0 -1 9 2 4 3))))

(check-report)
