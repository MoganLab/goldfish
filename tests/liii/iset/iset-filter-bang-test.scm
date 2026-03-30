(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-filter!
;; 与 iset-filter 相同，但可以修改原集合。
;;
(check (iset->list (iset-filter! (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
       => '(2 3 5)
) ;check

(check-report)
