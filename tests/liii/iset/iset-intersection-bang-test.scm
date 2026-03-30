(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-intersection!
;; 与 iset-intersection 相同，但可以修改第一个集合。
;;
(check (iset->list (iset-intersection! (iset 0 1 3 4) (iset 0 2 4)))
       => '(0 4)
) ;check

(check-report)
