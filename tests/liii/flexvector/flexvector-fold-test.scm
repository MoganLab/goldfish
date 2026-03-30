(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-fold
;; 折叠操作。
;;
;; 语法
;; ----
;; (flexvector-fold proc nil fv)
;; (flexvector-fold proc nil fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-fold (lambda (acc x) (cons x acc)) '() fv)
         => '(30 20 10)))

(check-report)
