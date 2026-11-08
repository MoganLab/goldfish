;;
;; iset>?
;; 检查第一个集合是否为第二个集合的真超集。
;;

;; 语法
;; ----
;; (iset>? iset1 iset2 ...)
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
;; boolean
;; set1 严格大于 set2 时返回 #t，否则返回 #f。

(import (liii check) (liii list) (srfi srfi-217))


(check-set-mode! 'report-failed)


(define pos-seq (iota 20 100 3))

(define pos-set (list->iset pos-seq))

(define pos-set+ (iset-adjoin pos-set 9))


(check-true (iset>? pos-set+ pos-set))
(check-false (iset>? pos-set pos-set))


(check-report)
