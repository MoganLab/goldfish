(import (liii check)
        (liii flexvector))

(check-set-mode! 'report-failed)

;; flexvector=?
;; 比较两个或多个可变长向量是否相等。
;;
;; 语法
;; ----
;; (flexvector=? eq? fv1 fv2 ...)
;;
(check-true (flexvector=? eq? (flexvector 'a 'b) (flexvector 'a 'b)))
(check-false (flexvector=? eq? (flexvector 'a 'b) (flexvector 'b 'a)))
(check-false (flexvector=? = (flexvector 1 2 3 4 5) (flexvector 1 2 3 4)))
(check-true (flexvector=? eq?))
(check-true (flexvector=? eq? (flexvector 'a)))

(check-report)
