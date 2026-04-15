(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-remove
;; 返回仅包含不满足谓词元素的新集合。
;;
;; 语法
;; ----
;; (iset-remove predicate iset)
;;
(check (iset->list (iset-remove (lambda (x) (< x 6))
                     (iset 2 3 5 7 11)
                   ) ;iset-remove
       ) ;iset->list
  =>
  '(7 11)
) ;check


(check-report)
