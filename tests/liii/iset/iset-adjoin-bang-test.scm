(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-adjoin!
;; 与 iset-adjoin 相同，但可以修改并返回原集合。
;;
(check (iset->list (iset-adjoin! (iset 1 3 5) 0)) => '(0 1 3 5))

(check-report)
