(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-delete-min!
;; 与 iset-delete-min 相同，但可以修改原集合。
;;
(let-values (((n set) (iset-delete-min! (iset 2 3 5 7 11))))
  (check n => 2)
  (check (iset->list set) => '(3 5 7 11))
) ;let-values

(check-report)
