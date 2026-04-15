(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-closed-open-interval
;; 返回集合中在左闭右开区间 [low, high) 内的元素。
;;
;; 语法
;; ----
;; (iset-closed-open-interval iset low high)
;;
(check (iset->list (iset-closed-open-interval (iset 2 3 5 7 11)
                     2
                     7
                   ) ;iset-closed-open-interval
       ) ;iset->list
  =>
  '(2 3 5)
) ;check


(check-report)
