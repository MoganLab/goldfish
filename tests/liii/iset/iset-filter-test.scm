(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-filter
;; 返回仅包含满足谓词元素的新集合。
;;
;; 语法
;; ----
;; (iset-filter predicate iset)
;;
(check (iset->list (iset-filter (lambda (x) (< x 6))
                     (iset 2 3 5 7 11)
                   ) ;iset-filter
       ) ;iset->list
  =>
  '(2 3 5)
) ;check


(check-report)
