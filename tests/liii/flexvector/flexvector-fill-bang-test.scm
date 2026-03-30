(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-fill!
;; 填充可变长向量。
;;
;; 语法
;; ----
;; (flexvector-fill! fv fill)
;; (flexvector-fill! fv fill start)
;; (flexvector-fill! fv fill start end)
;;
(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'x)
  (check (flexvector->list fv) => '(x x x x x)))

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'y 2)
  (check (flexvector->list fv) => '(1 2 y y y)))

(let ((fv (flexvector 1 2 3 4 5)))
  (flexvector-fill! fv 'z 1 3)
  (check (flexvector->list fv) => '(1 z z 4 5)))

(check-report)
