(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define comp (bag-comparator b-empty))
(define b-1-2 (bag 1 2 2))

;; bag-copy 函数测试
;;
;; 语法
;; ----
;; (bag-copy bag)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; 返回值
;; -----
;; 返回一个新的 bag，包含原 bag 的所有元素，比较器相同。

(let ((copy (bag-copy b-1-2)))
  (check-true (bag? copy))
  (check-false (eq? copy b-1-2))
  (check-true (eq? (bag-comparator copy) comp))
  (check (bag-size copy) => 3)
  (check (bag-count (lambda (x) (= x 2)) copy) => 2))
(check-true (bag-empty? (bag-copy b-empty)))
(check-catch 'type-error (bag-copy "not a bag"))

(check-report)
