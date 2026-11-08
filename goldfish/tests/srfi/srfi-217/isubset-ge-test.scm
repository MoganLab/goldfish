;;
;; isubset>=
;; 返回集合中大于或等于 k 的元素。
;;

;; 语法
;; ----
;; (isubset>= iset k)
;;

;; 参数
;; ----
;; set : iset
;; 目标集合。
;;
;; k : exact-integer
;; 目标元素。

;; 返回值
;; ----
;; iset
;; 返回至多包含一个元素 k 的 iset。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define mixed-seq (iota 20 -10 3))

(define pos-set (list->iset pos-seq))

(define mixed-set (list->iset mixed-seq))


(check (iset->list (isubset>= pos-set 148)) => '(148 151 154 157))
(check (iset->list (isubset>= mixed-set 38)) => '(38 41 44 47))


(check-report)
