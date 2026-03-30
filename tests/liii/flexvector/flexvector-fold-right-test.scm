(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-fold-right
;; 右折叠操作。
;;
;; 语法
;; ----
;; (flexvector-fold-right proc nil fv)
;; (flexvector-fold-right proc nil fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-fold-right (lambda (acc x) (cons x acc)) '() fv)
         => '(10 20 30)))

(check-report)
