(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; find 函数测试
;;
;; 在列表中查找第一个满足谓词的元素。
;;
;; 语法
;; ----
;; (find pred clist)
;;
;; 参数
;; ----
;; pred : procedure?
;; 一个谓词过程，接受列表中的每个元素作为参数，返回布尔值。
;;
;; clist : list?
;; 要查找的列表。
;;
;; 返回值
;; ------
;; value
;; 返回 clist 中第一个使 pred 返回 #t 的元素。如果没有找到，返回 #f。
;;
;; 注意
;; ----
;; find 返回 #f 时有语义上的歧义：无法区分是找到一个值为 #f 的元素，还是没有任何元素满足谓词。
;; 在大多数情况下，这种歧义不会出现。如果需要消除这种歧义，建议使用 find-tail。
;;
;; 错误处理
;; --------
;; wrong-type-arg 如果 clist 不是列表类型。
;;
;; 示例
;; ----
;; (find even? '(3 1 4 1 5 9)) => 4
;; (find even? '()) => #f
;; (find even? '(1 3 5 7 9)) => #f


(check (find even? '(3 1 4 1 5 9)) => 4)


(check (find even? '()) => #f)


(check (find even? '(1 3 5 7 9)) => #f)


;; 元素本身为 #f 时命中，返回值与未命中存在歧义
(check (find not '(1 #f 3)) => #f)


;; 点列表：在到达非正规尾部前命中，正常返回
(check (find even? '(1 2 . 3)) => 2)


;; 非列表参数，抛出 wrong-type-arg
(check-catch 'wrong-type-arg (find even? 3))


;; 点列表：遍历到非正规尾部仍未命中，抛出 wrong-type-arg
(check-catch 'wrong-type-arg (find odd? '(2 . 3)))


(check-report)
