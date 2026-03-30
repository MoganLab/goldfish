(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector-empty?
;; 检查可变长向量是否为空。
;;
;; 语法
;; ----
;; (flexvector-empty? fv)
;;
(check-true (flexvector-empty? (flexvector)))
(check-false (flexvector-empty? (flexvector 1 2 3)))

(check-report)
