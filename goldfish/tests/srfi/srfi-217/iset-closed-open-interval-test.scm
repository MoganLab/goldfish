;;
;; iset-closed-open-interval
;; 返回集合中在左闭右开区间 [low, high) 内的元素。
;;

;; 语法
;; ----
;; (iset-closed-open-interval iset low high)
;;

;; 参数
;; ----
;; set : iset
;; 目标集合。
;;
;; low : exact-integer
;; 区间下界。
;;
;; high : exact-integer
;; 区间上界。

;; 返回值
;; ----
;; iset
;; 返回半开区间 [low, high) 内元素组成的新 iset。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset->list (iset-closed-open-interval (iset 2 3 5 7 11) 2 7))
  =>
  '(2 3 5)
) ;check


(check-report)
