(import (liii check) (liii vector))

(check-set-mode! 'report-failed)

;; subvector 基本功能
(let ((v #(1 2 3 4 5 6)))
  (check (subvector? (subvector v 0 3)) => #t)
  (check (subvector-position (subvector v 2 4)) => 2)
  (check (vector->list (subvector v 1 4)) => '(2 3 4))
  (check (vector-dimensions (subvector v 0 6 '(3 2))) => '(3 2))
  (check (vector-ref (subvector v 0 6 '(3 2)) 1 1) => 4)
) ;let

;; 修改 subvector 元素会反映到原 vector（共享存储）
(let ((v (vector 1 2 3 4)))
  (let ((sub (subvector v 1 3)))
    (vector-set! sub 0 99)
    (check (vector-ref v 1) => 99)
  ) ;let
) ;let

;; 边界错误
(let ((v #(1 2 3)))
  (check-catch 'wrong-type-arg (subvector 'not-a-vector))
  (check-catch 'out-of-range (subvector v -1 2))
  (check-catch 'out-of-range (subvector v 0 4))
  (check-catch 'out-of-range (subvector v 2 1))
) ;let

(check-report)
