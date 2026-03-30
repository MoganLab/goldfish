(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-filter
;; 过滤操作。
;;
;; 语法
;; ----
;; (flexvector-filter pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-filter (lambda (x) (< x 25)) fv))
         => #(10 20)))

(check-report)
