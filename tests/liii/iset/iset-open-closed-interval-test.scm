(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-open-closed-interval
;; 返回集合中在左开右闭区间 (low, high] 内的元素。
;;
;; 语法
;; ----
;; (iset-open-closed-interval iset low high)
;;
(check (iset->list (iset-open-closed-interval (iset 2 3 5 7 11) 2 7))
       => '(3 5 7)
) ;check

(check-report)
