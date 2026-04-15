(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-member
;; 查找集合中与指定元素相等的元素。
;;
;; 语法
;; ----
;; (iset-member iset element default)
;;
;; 参数
;; ----
;; iset : iset
;; 要检查的集合。
;;
;; element : exact-integer
;; 要查找的元素。
;;
;; default : any
;; 如果 element 不在集合中，返回的值。
;;
;; 返回值
;; -----
;; 如果 element 在 iset 中，返回该元素；否则返回 default。
;;
(check (iset-member (iset 2 3 5 7 11) 7 #f)
  =>
  7
) ;check
(check (iset-member (iset 2 3 5 7 11)
         4
         'failure
       ) ;iset-member
  =>
  'failure
) ;check


(check-report)
