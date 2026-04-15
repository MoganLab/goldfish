(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-filter!
;; 与 iset-filter 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-filter! predicate iset)
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
;; 返回仅包含满足谓词元素的修改后的 iset。
;;
(check (iset->list (iset-filter! (lambda (x) (< x 6))
                     (iset 2 3 5 7 11)
                   ) ;iset-filter!
       ) ;iset->list
  =>
  '(2 3 5)
) ;check


(check-report)
