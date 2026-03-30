(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; list->iset!
;; 将列表元素并入集合。
;;
;; 语法
;; ----
;; (list->iset! iset list)
;;
;; 返回值
;; -----
;; 返回包含原集合和列表元素的 iset。可以修改原集合。
;;
(check (iset->list (list->iset! (iset 2 3 5) '(-3 -1 0)))
       => '(-3 -1 0 2 3 5)
) ;check

(check-report)
