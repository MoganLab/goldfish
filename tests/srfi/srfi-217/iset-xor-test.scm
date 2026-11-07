;;
;; iset-xor
;; 返回两个集合的对称差集。
;;

;; 语法
;; ----
;; (iset-xor iset1 iset2)
;;

;; 参数
;; ----
;; set1 : iset
;; 第一个集合。
;;
;; set2 : iset
;; 第二个集合。

;; 返回值
;; ----
;; iset
;; 返回只属于其中一个集合的元素组成的新 iset。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define pos-set (list->iset pos-seq))


(check (iset->list (iset-xor (iset 0 1 3) (iset 0 2 4))) => '(1 2 3 4))
(check (iset->list (iset-xor pos-set pos-set)) => '())


(check-report)
