(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-union!
;; 与 iset-union 相同，但可以修改第一个集合。
;;
(check (iset->list (iset-union! (iset 0 1 3) (iset 0 2 4)))
       => '(0 1 2 3 4)
) ;check

(check-report)
