;;
;; iset-open-closed-interval
;; 返回集合中在左开右闭区间 (low, high] 内的元素。
;;

;; 语法
;; ----
;; (iset-open-closed-interval iset low high)
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
;; 返回半开区间 (low, high] 内元素组成的新 iset。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset->list (iset-open-closed-interval (iset 2 3 5 7 11) 2 7))
  =>
  '(3 5 7)
) ;check


(check-report)
