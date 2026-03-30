(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-remove-range!
;; 移除指定范围内的元素。
;;
;; 语法
;; ----
;; (flexvector-remove-range! fv start end)
;;
(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 4)
  (check (flexvector->list fv) => '(a e f)))

(let ((fv (flexvector 'a 'b 'c 'd 'e 'f)))
  (flexvector-remove-range! fv 1 1)
  (check (flexvector->list fv) => '(a b c d e f)))

(check-report)
