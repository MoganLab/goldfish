(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-concatenate
;; 连接可变长向量列表。
;;
;; 语法
;; ----
;; (flexvector-concatenate list)
;;
(check (flexvector->vector
         (flexvector-concatenate
           (list (flexvector 10 20) (flexvector) (flexvector 30 40))))
       => #(10 20 30 40))

(check-report)
