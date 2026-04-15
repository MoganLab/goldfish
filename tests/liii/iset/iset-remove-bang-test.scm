(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-remove!
;; 与 iset-remove 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-remove! predicate iset)
;;
;; 参数
;; ----
;; predicate : procedure
;; 接受一个整数并返回布尔值的函数。
;;
;; iset : iset
;; 要修改的集合。
;;
;; 返回值
;; -----
;; 返回仅包含不满足谓词元素的修改后的 iset。
;;
(check (iset->list (iset-remove! (lambda (x) (< x 6))
                     (iset 2 3 5 7 11)
                   ) ;iset-remove!
       ) ;iset->list
  =>
  '(7 11)
) ;check


(check-report)
