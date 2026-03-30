(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-unfold-right
;; 右展开操作。
;;
;; 语法
;; ----
;; (flexvector-unfold-right pred? gen succ seed)
;;
(check (flexvector->vector
         (flexvector-unfold-right (lambda (x) (> x 10))
                                  (lambda (x) (* x x))
                                  (lambda (x) (+ x 1))
                                  1))
       => #(100 81 64 49 36 25 16 9 4 1))

(check-report)
