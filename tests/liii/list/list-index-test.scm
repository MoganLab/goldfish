(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; list-index 函数测试
;;
;; 语法
;; ----
;; (list-index pred clist)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数，接受单个参数并返回布尔值。
;;
;; clist : list?
;; 要查找的列表。
;;
;; 返回值
;; ------
;; integer? 或 #f
;; 返回第一个满足谓词条件的元素的索引（从0开始），如果没有找到则返回#f。
;;
;; 示例
;; ----
;; (list-index even? '(3 1 4 1 5 9)) => 2
;; (list-index even? '()) => #f
;; (list-index even? '(1 3 5 7 9)) => #f


(check (list-index even? '(3 1 4 1 5 9))
  =>
  2
) ;check
(check (list-index even? '()) => #f)
(check (list-index even? '(1 3 5 7 9))
  =>
  #f
) ;check


(check-report)
