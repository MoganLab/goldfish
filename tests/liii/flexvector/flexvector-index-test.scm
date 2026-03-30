(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-index
;; 查找元素索引。
;;
;; 语法
;; ----
;; (flexvector-index pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-index (lambda (x) (> x 10)) fv) => 1))

(check-report)
