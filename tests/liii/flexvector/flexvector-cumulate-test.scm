(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-cumulate
;; 累积计算。
;;
;; 语法
;; ----
;; (flexvector-cumulate proc nil fv)
;;
(check (flexvector->vector
         (flexvector-cumulate + 0 (flexvector 3 1 4 1 5 9 2 5 6)))
       => #(3 4 8 9 14 23 25 30 36))

(check-report)
