(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-closed-interval
;; 返回集合中在闭区间 [low, high] 内的元素。
;;
;; 语法
;; ----
;; (iset-closed-interval iset low high)
;;
;; 参数
;; ----
;; low, high : exact-integer
;; 区间边界（包含）。
;;
(check (iset->list (iset-closed-interval (iset 2 3 5 7 11) 2 7))
       => '(2 3 5 7)
) ;check

(check-report)
