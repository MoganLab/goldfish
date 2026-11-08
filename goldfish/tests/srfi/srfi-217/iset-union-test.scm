;;
;; iset-union
;; 返回多个集合的并集。
;;

;; 语法
;; ----
;; (iset-union iset1 iset2 ...)
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
;; 返回两个集合并集组成的新 iset。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define neg-seq (iota 20 -100 3))

(define pos-set (list->iset pos-seq))

(define neg-set (list->iset neg-seq))


(check (iset->list (iset-union (iset 0 1 3) (iset 0 2 4))) => '(0 1 2 3 4))
(check (iset->list (iset-union pos-set neg-set))
  =>
  (iset->list (list->iset (append pos-seq neg-seq)))
) ;check


(check-report)
