(import (liii check)
  (liii bag)
  (liii error)
) ;import

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))

;; bag-empty? 函数测试
;;
;; 语法
;; ----
;; (bag-empty? bag)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; 返回值
;; -----
;; 如果 bag 为空，返回 #t；否则返回 #f。

(check-true (bag-empty? b-empty))
(check-false (bag-empty? b-1-2))
(check-catch 'type-error
  (bag-empty? "not a bag")
) ;check-catch

(check-report)
