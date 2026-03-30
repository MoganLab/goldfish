(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-delete-max!
;; 与 iset-delete-max 相同，但可以修改原集合。
;;
(let-values (((n set) (iset-delete-max! (iset 2 3 5 7 11))))
  (check n => 11)
  (check (iset->list set) => '(2 3 5 7))
) ;let-values

(check-report)
