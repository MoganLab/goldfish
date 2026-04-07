(import (liii check)
        (liii flexvector)
) ;import

(check-set-mode! 'report-failed)

;; reverse-flexvector->list
;; 可变长向量反向转换为列表。
;;
;; 语法
;; ----
;; (reverse-flexvector->list fv)
;;
(let ((fv (flexvector 1 2 3)))
  (check (reverse-flexvector->list fv) => '(3 2 1))
) ;let

(check-report)
