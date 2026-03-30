(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-difference!
;; 与 iset-difference 相同，但可以修改第一个集合。
;;
(check (iset->list (iset-difference! (iset 0 1 3 4) (iset 0 2) (iset 0 4)))
       => '(1 3)
) ;check

(check-report)
