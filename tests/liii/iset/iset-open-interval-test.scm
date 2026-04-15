(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define neg-set
  (list->iset (iota 20 -100 3))
) ;define


;;
;; iset-open-interval
;; 返回集合中在开区间 (low, high) 内的元素。
;;
;; 语法
;; ----
;; (iset-open-interval iset low high)
;;
;; 参数
;; ----
;; iset : iset
;; 源集合。
;;
;; low, high : exact-integer
;; 区间边界（不包含）。
;;
(check (iset->list (iset-open-interval (iset 2 3 5 7 11)
                     2
                     7
                   ) ;iset-open-interval
       ) ;iset->list
  =>
  '(3 5)
) ;check
(check-true (iset-empty? (iset-open-interval neg-set 0 50)
            ) ;iset-empty?
) ;check-true


(check-report)
