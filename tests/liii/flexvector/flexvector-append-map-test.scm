(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-append-map
;; 映射并追加结果。
;;
;; 语法
;; ----
;; (flexvector-append-map proc fv)
;;
(check (flexvector->vector
         (flexvector-append-map (lambda (x) (flexvector x (* x 10)))
                                (flexvector 10 20 30))
         ) ;flexvector-append-map
       => #(10 100 20 200 30 300)
) ;check

(check-report)
