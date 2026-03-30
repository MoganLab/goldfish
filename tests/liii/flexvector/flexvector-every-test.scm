(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-every
;; 检查是否全部元素满足条件。
;;
;; 语法
;; ----
;; (flexvector-every pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-every (lambda (x) (< x 40)) fv) => #t)
  (check (flexvector-every (lambda (x) (< x 30)) fv) => #f))

(check-report)
