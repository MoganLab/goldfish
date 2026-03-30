(import (liii check)
        (liii iset)
) ;import

(check-set-mode! 'report-failed)

;;
;; iset-every?
;; 检查集合中是否所有元素都满足谓词。
;;
;; 语法
;; ----
;; (iset-every? predicate iset)
;;
;; 返回值
;; -----
;; 如果所有元素都满足谓词，返回 #t；否则返回 #f。
;; 注意：空集合返回 #t。
;;
(check-true (iset-every? (lambda (x) (< x 5)) (iset -2 -1 1 2)))
(check-false (iset-every? positive? (iset -2 -1 1 2)))
(check-true (iset-every? even? (iset)))

(check-report)
