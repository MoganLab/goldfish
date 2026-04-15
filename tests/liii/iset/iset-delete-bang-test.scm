(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-delete!
;; 与 iset-delete 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-delete! iset element ...)
;;
;; 参数
;; ----
;; iset : iset
;; 要修改的集合。
;;
;; element ... : exact-integer
;; 要移除的元素。
;;
;; 返回值
;; -----
;; 返回修改后的原 iset。
;;
(check (iset->list (iset-delete! (iset 1 3 5) 3)
       ) ;iset->list
  =>
  '(1 5)
) ;check


(check-report)
