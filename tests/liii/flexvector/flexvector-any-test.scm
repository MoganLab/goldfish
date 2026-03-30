(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-any
;; 检查是否存在满足条件的元素。
;;
;; 语法
;; ----
;; (flexvector-any pred? fv)
;;
(let ((fv (flexvector 10 20 30)))
  (check (flexvector-any (lambda (x) (= x 20)) fv) => #t)
  (check (flexvector-any (lambda (x) (= x 21)) fv) => #f))

(check-report)
