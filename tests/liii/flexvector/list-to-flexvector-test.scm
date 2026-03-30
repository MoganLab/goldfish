(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; list->flexvector
;; 列表转换为可变长向量。
;;
;; 语法
;; ----
;; (list->flexvector list)
;;
(check (flexvector->list (list->flexvector '(a b c))) => '(a b c))

(check-report)
