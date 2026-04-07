(import (liii check)
        (liii bag)
        (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

;; bag-contains? 函数测试
;;
;; 语法
;; ----
;; (bag-contains? bag element)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; element : any
;; 要检查的元素。
;;
;; 返回值
;; -----
;; 如果 bag 中存在与 element 等价的元素，返回 #t；否则返回 #f。

(check-true (bag-contains? b-1-2 2))
(check-false (bag-contains? b-1-2 9))
(check-false (bag-contains? b-empty 1))
(check-catch 'type-error (bag-contains? "not a bag" 1))

(check-report)
