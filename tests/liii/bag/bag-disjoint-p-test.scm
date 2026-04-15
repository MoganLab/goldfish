(import (liii check)
  (liii bag)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))

;; bag-disjoint? 函数测试
;;
;; 语法
;; ----
;; (bag-disjoint? bag1 bag2)
;;
;; 参数
;; ----
;; bag1 : bag
;; 第一个 bag。
;;
;; bag2 : bag
;; 第二个 bag。
;;
;; 返回值
;; -----
;; 如果两个 bag 没有相等元素，返回 #t；否则返回 #f。

(check-true (bag-disjoint? (bag 1 1) (bag 2 2))
) ;check-true
(check-false (bag-disjoint? (bag 1 1) (bag 1 2))
) ;check-false
(check-true (bag-disjoint? b-empty (bag 1))
) ;check-true
(check-true (bag-disjoint? (bag 1) b-empty)
) ;check-true
(check-catch 'type-error
  (bag-disjoint? "not a bag" (bag 1))
) ;check-catch
(check-catch 'type-error
  (bag-disjoint? (bag 1) "not a bag")
) ;check-catch

(check-report)
