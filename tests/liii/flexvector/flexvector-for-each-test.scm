(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-for-each
;; 遍历可变长向量。
;;
;; 语法
;; ----
;; (flexvector-for-each proc fv)
;; (flexvector-for-each proc fv1 fv2 ...)
;;
(let ((fv (flexvector 10 20 30))
      (res '()))
  (flexvector-for-each (lambda (x) (set! res (cons x res))) fv)
  (check res => '(30 20 10)))

(check-report)
