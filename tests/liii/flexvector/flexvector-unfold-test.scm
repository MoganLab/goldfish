(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-unfold
;; 展开操作。
;;
;; 语法
;; ----
;; (flexvector-unfold pred? gen succ seed)
;;
(check (flexvector->vector
         (flexvector-unfold (lambda (x) (> x 10))
                            (lambda (x) (* x x))
                            (lambda (x) (+ x 1))
                            1))
       => #(1 4 9 16 25 36 49 64 81 100))

(check-report)
