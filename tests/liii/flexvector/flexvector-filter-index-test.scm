(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-filter/index
;; 带索引过滤操作。
;;
;; 语法
;; ----
;; (flexvector-filter/index pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check
    (flexvector->vector
      (flexvector-filter/index (lambda (i x) (not (= i 1))) fv)
    ) ;flexvector->vector
         => #(10 30)
  ) ;check
) ;let

(check-report)
