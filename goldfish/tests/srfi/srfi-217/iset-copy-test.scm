;;
;; iset-copy
;; 复制一个集合。
;;

;; 语法
;; ----
;; (iset-copy iset)
;;

;; 参数
;; ----
;; set : iset
;; 待复制的集合。

;; 返回值
;; ----
;; 返回包含相同元素的新 iset。
;;

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define pos-set (list->iset pos-seq))


(check-true (not (eqv? (iset-copy pos-set) pos-set)))
(check-true (iset=? (iset-copy pos-set) pos-set))


(check-report)
