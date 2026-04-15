(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-delete-max!
;; 与 iset-delete-max 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-delete-max! iset)
;;
;; 参数
;; ----
;; iset : iset
;; 要修改的集合。
;;
;; 返回值
;; -----
;; 返回两个值：最大元素和包含其余元素的修改后的集合。
;; 如果集合为空则报错。
;;
(let-values (((n set)
              (iset-delete-max! (iset 2 3 5 7 11))
             ) ;
            ) ;
  (check n => 11)
  (check (iset->list set) => '(2 3 5 7))
) ;let-values


(check-report)
