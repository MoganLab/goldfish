(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-partition!
;; 与 iset-partition 相同，但可以修改原集合。
;;
;; 语法
;; ----
;; (iset-partition! predicate iset)
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
;; 返回两个值：满足谓词的集合和不满足谓词的集合（均为修改后的原集合）。
;;
(let-values (((low high)
              (iset-partition! (lambda (x) (< x 6))
                (iset 2 3 5 7 11)
              ) ;iset-partition!
             ) ;
            ) ;
  (check (iset->list low) => '(2 3 5))
  (check (iset->list high) => '(7 11))
) ;let-values


(check-report)
