(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-reverse!
;; 反转可变长向量（破坏性）。
;;
;; 语法
;; ----
;; (flexvector-reverse! fv)
;; (flexvector-reverse! fv start)
;; (flexvector-reverse! fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (flexvector-reverse! fv)
  (check (flexvector->list fv) => '(3 2 1)))

(check-report)
