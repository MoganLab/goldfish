(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-index-right
;; 从右查找元素索引。
;;
;; 语法
;; ----
;; (flexvector-index-right pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index-right (lambda (x) (> x 10))
           fv
         ) ;flexvector-index-right
    =>
    2
  ) ;check
) ;let


(check-report)
