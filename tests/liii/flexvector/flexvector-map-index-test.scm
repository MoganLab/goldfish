(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-map/index
;; 带索引映射操作。
;;
;; 语法
;; ----
;; (flexvector-map/index proc fv)
;; (flexvector-map/index proc fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector->vector
           (flexvector-map/index (lambda (i x) (+ x (* i 2))) fv))
         => #(10 22 34)))

(check-report)
