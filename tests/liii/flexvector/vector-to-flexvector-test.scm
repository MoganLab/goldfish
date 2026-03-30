(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; vector->flexvector
;; 向量转换为可变长向量。
;;
;; 语法
;; ----
;; (vector->flexvector vec)
;; (vector->flexvector vec start)
;; (vector->flexvector vec start end)
;;
(check (flexvector->vector (vector->flexvector #(1 2 3))) => #(1 2 3))

(check-report)
