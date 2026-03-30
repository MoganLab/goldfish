(import (liii check)
        (liii iset)
        (only (srfi srfi-1) count)
) ;import

(check-set-mode! 'report-failed)

(define pos-seq (iota 20 100 3))
(define pos-set (list->iset pos-seq))

;;
;; iset-count
;; 计算集合中满足谓词的元素数量。
;;
;; 语法
;; ----
;; (iset-count predicate iset)
;;
(check (iset-count positive? (iset -2 -1 1 2)) => 2)
(check (iset-count even? (iset)) => 0)
(check (iset-count even? pos-set) => (count even? pos-seq))

(check-report)
