(import (liii check) (liii flexvector))


(check-set-mode! 'report-failed)


;; flexvector-append
;; 连接可变长向量。
;;
;; 语法
;; ----
;; (flexvector-append fv ...)
;;
(check (flexvector->vector (flexvector-append (flexvector 10 20)
                             (flexvector)
                             (flexvector 30 40)
                           ) ;flexvector-append
       ) ;flexvector->vector
  =>
  #(10 20 30 40)
) ;check


(check-report)
