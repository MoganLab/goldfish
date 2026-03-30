(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; list->iset
;; 将列表转换为集合。
;;
;; 语法
;; ----
;; (list->iset list)
;;
;; 参数
;; ----
;; list : list of exact-integers
;; 要转换的列表。
;;
;; 返回值
;; -----
;; 返回包含列表元素的新 iset。重复元素会被去重。
;;
(check (iset->list (list->iset '(-3 -1 0 2))) => '(-3 -1 0 2))

(check-report)
