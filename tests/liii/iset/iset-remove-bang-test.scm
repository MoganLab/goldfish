(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-remove!
;; 与 iset-remove 相同，但可以修改原集合。
;;
(check (iset->list (iset-remove! (lambda (x) (< x 6)) (iset 2 3 5 7 11)))
       => '(7 11)
) ;check

(check-report)
