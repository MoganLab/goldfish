(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-filter!
;; 破坏性过滤操作。
;;
;; 语法
;; ----
;; (flexvector-filter! pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (flexvector-filter! (lambda (x) (< x 25)) fv)
  (check (flexvector->list fv) => '(10 20))
) ;let

(check-report)
