(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-skip
;; 跳过满足条件的元素。
;;
;; 语法
;; ----
;; (flexvector-skip pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-skip (lambda (x) (< x 25)) fv) => 2))

(check-report)
