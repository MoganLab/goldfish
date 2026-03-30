(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-delete-all!
;; 与 iset-delete-all 相同，但可以修改原集合。
;;
(check (iset->list (iset-delete-all! (iset 2 3 5 7 11) '(3 4 5))) => '(2 7 11))

(check-report)
