;;
;; iset-delete-all
;; 与 iset-delete 相同，但接受一个元素列表。
;;

;; 语法
;; ----
;; (iset-delete-all iset element-list)
;;

;; 参数
;; ----
;; set : iset
;; 目标集合。
;;
;; ns : exact-integer ...
;; 待删除的元素。

;; 返回值
;; ----
;; iset
;; 返回删除给定元素后的新 iset。

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(check (iset->list (iset-delete-all (iset 2 3 5 7 11) '(3 4 5))) => '(2 7 11))


(check-report)
