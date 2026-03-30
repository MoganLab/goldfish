(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-clear!
;; 清空可变长向量。
;;
;; 语法
;; ----
;; (flexvector-clear! fv)
;;
(let ((fv (flexvector 'a 'b 'c)))
  (flexvector-clear! fv)
  (check (flexvector-length fv) => 0)
  (check (flexvector-empty? fv) => #t))

(check-report)
