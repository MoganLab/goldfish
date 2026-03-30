(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-delete-all
;; 与 iset-delete 相同，但接受一个元素列表。
;;
;; 语法
;; ----
;; (iset-delete-all iset element-list)
;;
(check (iset->list (iset-delete-all (iset 2 3 5 7 11) '(3 4 5))) => '(2 7 11))

(check-report)
