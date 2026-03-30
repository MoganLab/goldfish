(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector->list
;; 可变长向量转换为列表。
;;
;; 语法
;; ----
;; (flexvector->list fv)
;; (flexvector->list fv start)
;; (flexvector->list fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (check (flexvector->list fv) => '(1 2 3)))

(check-report)
