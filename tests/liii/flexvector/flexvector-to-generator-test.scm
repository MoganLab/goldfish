(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector->generator
;; 可变长向量转换为生成器。
;;
;; 语法
;; ----
;; (flexvector->generator fv)
;;
(let ((gen (flexvector->generator (flexvector 'a 'b 'c))))
  (check (gen) => 'a)
  (check (gen) => 'b)
  (check (gen) => 'c)
  (check (eof-object? (gen)) => #t))

(check-report)
