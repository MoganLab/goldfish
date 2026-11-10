;; find
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
;; any
;; 返回 clist 中第一个使 pred 返回真值的元素。如果没有找到，返回 #f。

(import (srfi srfi-1) (liii check))

(check-set-mode! 'report-failed)

(check (find even? '(3 1 4 1 5 9)) => 4)
(check (find even? '()) => #f)
(check (find even? '(1 3 5 7 9)) => #f)

;; 元素本身为 #f 时命中，返回值与未命中存在歧义
(check (find not '(1 #f 3)) => #f)

;; 点列表：在到达非正规尾部前命中，正常返回
(check (find even? '(1 2 . 3)) => 2)

;; 非列表参数，抛出 type-error
(check-catch 'type-error (find even? 3))

;; 点列表：遍历到非正规尾部仍未命中，抛出 type-error
(check-catch 'type-error (find odd? '(2 . 3)))

(check-report)
