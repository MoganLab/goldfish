(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; flexvector-append-subvectors
;; 连接可变长向量的子向量。
;;
;; 语法
;; ----
;; (flexvector-append-subvectors fv1 start1 end1 fv2 start2 end2 ...)
;;
(check (flexvector->vector
         (flexvector-append-subvectors
           (flexvector 'a 'b 'c 'd 'e) 0 2
           (flexvector 'f 'g 'h 'i 'j) 2 4)
         ) ;flexvector-append-subvectors
       => #(a b h i)
) ;check

(check-report)
