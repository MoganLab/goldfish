(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-delete!
;; 与 iset-delete 相同，但可以修改原集合。
;;
(check (iset->list (iset-delete! (iset 1 3 5) 3)) => '(1 5))

(check-report)
