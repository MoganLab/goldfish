(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-delete
;; 返回一个新集合，移除指定元素。
;;
;; 语法
;; ----
;; (iset-delete iset element ...)
;;
;; 参数
;; ----
;; iset : iset
;; 初始集合。
;;
;; element ... : exact-integer
;; 要移除的元素。
;;
;; 返回值
;; -----
;; 返回一个新的 iset。
;;
(check (iset->list (iset-delete (iset 1 3 5) 3)
       ) ;iset->list
  =>
  '(1 5)
) ;check
(check (iset->list (iset-delete (iset 1 2 3) 4)
       ) ;iset->list
  =>
  '(1 2 3)
) ;check


(check-report)
