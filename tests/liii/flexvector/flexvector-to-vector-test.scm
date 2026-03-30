(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector->vector
;; 可变长向量转换为向量。
;;
;; 语法
;; ----
;; (flexvector->vector fv)
;; (flexvector->vector fv start)
;; (flexvector->vector fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->vector fv) => #(1 2 3)))

(check-report)
