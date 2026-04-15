(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-sort
;; 对列表进行非破坏性快速排序。
;;
;; 语法
;; ----
;; (list-sort cmp lst)
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
;; 排序后的新列表，原列表保持不变。
;;
;; 注意
;; ----
;; 这是一个非破坏性操作，原列表不会被修改。
;;
;; 示例
;; ----
;; (list-sort < '(3 1 4 1 5 9 2 6)) => '(1 1 2 3 4 5 6 9)
;;
;; 错误处理
;; ----
;; 无


(define test-list '(3 1 4 1 5 9 2 6 5))
(define sorted-list
  (list-sort < test-list)
) ;define


(check-true (list-sorted? < sorted-list)
) ;check-true
(check (length sorted-list)
  =>
  (length test-list)
) ;check
(check sorted-list
  =>
  '(1 1 2 3 4 5 5 6 9)
) ;check
(check (equal? test-list '(3 1 4 1 5 9 2 6 5))
  =>
  #t
) ;check


;; 边界情况
(check (list-sort < '()) => '())
(check (list-sort < '(42)) => '(42))
(check (list-sort < '(1 2 3 4 5))
  =>
  '(1 2 3 4 5)
) ;check
(check (list-sort > '(1 2 3 4 5))
  =>
  '(5 4 3 2 1)
) ;check


(check-report)
