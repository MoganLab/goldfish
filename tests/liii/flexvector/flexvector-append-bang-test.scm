(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-append!
;; 破坏性连接。
;;
;; 语法
;; ----
;; (flexvector-append! fv ...)
;;
(let ((fv (flexvector 10 20)))
  (flexvector-append! fv (flexvector 30 40))
  (check (flexvector->vector fv) => #(10 20 30 40)))

(check-report)
