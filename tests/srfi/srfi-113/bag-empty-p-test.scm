;; bag-empty?
;; 如果 bag 为空，返回 #t；否则返回 #f。
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
;; ----
;; 如果 bag 为空，返回 #t；否则返回 #f。

(import (liii check) (liii bag) (liii error))

(check-set-mode! 'report-failed)

;; Data Setup

(define b-empty (bag))

(define b-1-2 (bag 1 2 2))


(check-true (bag-empty? b-empty))
(check-false (bag-empty? b-1-2))
(check-catch 'type-error (bag-empty? "not a bag"))

(check-report)
