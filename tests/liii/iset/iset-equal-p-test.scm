(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset=?
;; 检查两个或多个集合是否相等（包含相同元素）。
;;
;; 语法
;; ----
;; (iset=? iset1 iset2 ...)
;;
(check-true (iset=? (iset) (iset)))
(check-true (iset=? (iset 1 2 3 4) (iset 2 1 4 3)))
(check-true (iset=? (iset 1 2 3 4) (iset 2 1 4 3) (iset 3 2 1 4)))
(check-false (iset=? (iset 1 2 3 4) (iset 2 3 4)))

(check-report)
