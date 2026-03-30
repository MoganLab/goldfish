(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-map!
;; 破坏性映射操作。
;;
;; 语法
;; ----
;; (flexvector-map! proc fv)
;; (flexvector-map! proc fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30)))
  (flexvector-map! (lambda (x) (* x 10)) fv)
  (check (flexvector->list fv) => '(100 200 300)))

(check-report)
