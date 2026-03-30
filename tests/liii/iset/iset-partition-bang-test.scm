(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-partition!
;; 与 iset-partition 相同，但可以修改原集合。
;;
(let-values (((low high) (iset-partition! (lambda (x) (< x 6))
                                           (iset 2 3 5 7 11))))
  (check (iset->list low) => '(2 3 5))
  (check (iset->list high) => '(7 11))
) ;let-values

(check-report)
