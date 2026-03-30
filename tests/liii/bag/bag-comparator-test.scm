(import (liii check)
        (liii bag)
        (liii error))

(check-set-mode! 'report-failed)

;; Data Setup
(define b-empty (bag))
(define b-1-2 (bag 1 2 2))
(define comp (bag-comparator b-empty))

;; bag-comparator 函数测试
;;
;; 语法
;; ----
;; (bag-comparator bag)
;;
;; 参数
;; ----
;; bag : bag
;; 目标 bag。
;;
;; 返回值
;; -----
;; 返回 bag 使用的 comparator。

(check-true (eq? (bag-comparator b-empty) comp))
(check-true (eq? (bag-comparator b-1-2) comp))

(check-report)
