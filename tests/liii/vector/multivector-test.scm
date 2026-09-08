(import (liii check) (liii vector))

(check-set-mode! 'report-failed)

;; multivector 通用多维向量字面量
(let ((m (eval-string "#2d((1 2 3) (4 5 6))")))
  (check (vector? m) => #t)
  (check (vector-dimensions m) => '(2 3))
  (check (vector-ref m 0 0) => 1)
  (check (vector-ref m 0 2) => 3)
  (check (vector-ref m 1 0) => 4)
  (check (vector-ref m 1 2) => 6)
) ;let

;; 3维向量字面量
(let ((m (eval-string "#3d(((1 2) (3 4)) ((5 6) (7 8)))")))
  (check (vector-dimensions m) => '(2 2 2))
  (check (vector-ref m 0 0 0) => 1)
  (check (vector-ref m 1 1 1) => 8)
) ;let

(check-report)
