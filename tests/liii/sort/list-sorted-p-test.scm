(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-sorted?
;; 检查列表是否已按指定比较函数排序。
;;
;; 语法
;; ----
;; (list-sorted? cmp lst)
;;
;; 参数
;; ----
;; cmp : procedure
;; 比较函数，接受两个参数，返回布尔值。
;;
;; lst : list
;; 要检查的列表。
;;
;; 返回值
;; ----
;; boolean
;; 如果列表已排序返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (list-sorted? < '(1 2 3 4 5)) => #t
;; (list-sorted? < '(1 5 1 0 -1)) => #f
;;
;; 错误处理
;; ----
;; 无


(check-false (list-sorted? < '(1 5 1 0 -1 9 2 4 3))
) ;check-false
(check-true (list-sorted? < '(1 2 3 4 5))
) ;check-true
(check-true (list-sorted? < '()))
(check-true (list-sorted? < '(42)))
(check-true (list-sorted? > '(5 4 3 2 1))
) ;check-true
(check-false (list-sorted? > '(1 2 3 4 5))
) ;check-false


;; 配合排序函数使用
(check-true (list-sorted? <
              (list-sort < '(1 5 1 0 -1 9 2 4 3))
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort <
                '(1 5 1 0 -1 9 2 4 3)
              ) ;list-stable-sort
            ) ;list-sorted?
) ;check-true


(check-report)
