(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-reverse-copy
;; 反向复制可变长向量。
;;
;; 语法
;; ----
;; (flexvector-reverse-copy fv)
;; (flexvector-reverse-copy fv start)
;; (flexvector-reverse-copy fv start end)
;;
(let ((fv (flexvector 1 2 3)))
  (let ((rev (flexvector-reverse-copy fv)))
    (check (flexvector->list rev) => '(3 2 1))
    (check (flexvector->list fv) => '(1 2 3))))

(check-report)
