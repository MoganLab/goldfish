(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-swap!
;; 交换两个位置的元素。
;;
;; 语法
;; ----
;; (flexvector-swap! fv i j)
;;
(let ((fv (flexvector 10 20 30)))
  (flexvector-swap! fv 0 2)
  (check (flexvector->list fv) => '(30 20 10)))

(check-report)
