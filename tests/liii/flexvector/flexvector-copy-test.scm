(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-copy
;; 复制可变长向量。
;;
;; 语法
;; ----
;; (flexvector-copy fv)
;; (flexvector-copy fv start)
;; (flexvector-copy fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (let ((copy (flexvector-copy fv)))
    (check (flexvector-length fv) => (flexvector-length copy))
    (check-false (eq? fv copy))
    (check (flexvector-ref copy 0) => 1)
    (flexvector-set! copy 0 'x)
    (check (flexvector-ref fv 0) => 1)
    (check (flexvector-ref copy 0) => 'x)))

(check-report)
