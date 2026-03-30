(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-copy!
;; 将源向量的内容复制到目标向量。
;;
;; 语法
;; ----
;; (flexvector-copy! to at from)
;; (flexvector-copy! to at from start)
;; (flexvector-copy! to at from start end)
;;
(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 20 30 40)))
  (flexvector-copy! to 1 from)
  (check (flexvector->list to) => '(1 20 30 40 5)))

(let ((to (flexvector 1 2 3 4 5))
      (from (flexvector 10 20 30 40 50)))
  (flexvector-copy! to 1 from 1 4)
  (check (flexvector->list to) => '(1 20 30 40 5)))

(check-report)
