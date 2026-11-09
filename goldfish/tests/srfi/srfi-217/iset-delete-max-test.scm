;;
;; iset-delete-max
;; 返回最大元素和剩余集合。
;;

;; 语法
;; ----
;; (iset-delete-max iset)
;;

;; 参数
;; ----
;; set : iset
;; 目标集合。

;; 返回值
;; ----
;; 返回两个值：最大元素和包含其余元素的新集合。
;; 如果集合为空则报错。
;;

(import (liii check) (srfi srfi-217))


(check-set-mode! 'report-failed)


(let-values (((n set) (iset-delete-max (iset 2 3 5 7 11))))
  (check n => 11)
  (check (iset->list set) => '(2 3 5 7))
) ;let-values


(check-report)
