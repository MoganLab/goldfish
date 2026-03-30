(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-count
;; 统计满足条件的元素数量。
;;
;; 语法
;; ----
;; (flexvector-count pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-count (lambda (x) (< x 25)) fv) => 2))

(check-report)
