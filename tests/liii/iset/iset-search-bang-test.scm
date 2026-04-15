(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


(define mixed-set
  (list->iset (iota 20 -10 3))
) ;define


;;
;; iset-search!
;; 与 iset-search 相同，但可以修改原集合。
;;
(call-with-values (lambda ()
                    (iset-search! (iset-copy mixed-set)
                      1
                      (lambda (insert _) (insert #t))
                      (lambda (x update _) (update 1 #t))
                    ) ;iset-search!
                  ) ;lambda
  (lambda (set _)
    (check-true (iset=? (iset-adjoin mixed-set 1) set)
    ) ;check-true
  ) ;lambda
) ;call-with-values


(check-report)
