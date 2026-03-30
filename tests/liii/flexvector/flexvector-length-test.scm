(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-length
;; 返回可变长向量的长度。
;;
;; 语法
;; ----
;; (flexvector-length fv)
;;
(check (flexvector-length (flexvector)) => 0)
(check (flexvector-length (flexvector 1 2 3)) => 3)

(check-report)
