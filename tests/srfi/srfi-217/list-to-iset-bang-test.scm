;;
;; list->iset!
;; 将列表元素并入集合。
;;

;; 语法
;; ----
;; (list->iset! iset list)
;;

;; 参数
;; ----
;; set : iset
;; 目标集合。
;;
;; ns : list
;; 待加入集合的元素列表。

;; 返回值
;; ----
;; 返回包含原集合和列表元素的 iset。可以修改原集合。
;;

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset->list (list->iset! (iset 2 3 5) '(-3 -1 0))) => '(-3 -1 0 2 3 5))


(check-report)
