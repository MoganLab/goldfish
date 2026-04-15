(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-delete-all!
;; 与 iset-delete-all 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-delete-all! iset element-list)
;;
;; 参数
;; ----
;; iset : iset
;; 要修改的集合。
;;
;; element-list : list
;; 要移除的元素列表。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-delete-all! (iset 2 3 5 7 11)
                     '(3 4 5)
                   ) ;iset-delete-all!
       ) ;iset->list
  =>
  '(2 7 11)
) ;check


(check-report)
