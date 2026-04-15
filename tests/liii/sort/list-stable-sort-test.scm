(import (liii check) (liii sort))


(check-set-mode! 'report-failed)


;; list-stable-sort
;; 对列表进行非破坏性稳定排序。
;;
;; 语法
;; ----
;; (list-stable-sort cmp lst)
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
;; 排序后的新列表，原列表保持不变。相等元素的相对顺序保持不变。
;;
;; 注意
;; ----
;; 这是一个非破坏性操作，原列表不会被修改。稳定排序保证相等元素的原始顺序。
;;
;; 示例
;; ----
;; (list-stable-sort < '(1 5 1 0 -1 9 2 4 3)) => 稳定排序后的列表
;;
;; 错误处理
;; ----
;; 无


(check-true (list-sorted? <
              (list-stable-sort <
                '(1 5 1 0 -1 9 2 4 3)
              ) ;list-stable-sort
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort <
                '(9 7 5 3 2 8 6 4 1)
              ) ;list-stable-sort
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort < '())
            ) ;list-sorted?
) ;check-true
(check-true (list-sorted? <
              (list-stable-sort < '(42))
            ) ;list-sorted?
) ;check-true


;; 测试稳定性（相等元素保持相对顺序）
(define pairs
  '((1 . a) (2 . b) (1 . c) (3 . d) (2 . e))
) ;define
(define sorted-pairs
  (list-stable-sort (lambda (x y) (< (car x) (car y)))
    pairs
  ) ;list-stable-sort
) ;define
(check sorted-pairs
  =>
  '((1 . a) (1 . c) (2 . b) (2 . e) (3 . d))
) ;check


(check-report)
