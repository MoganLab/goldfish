(import (liii check) (liii iset))


(check-set-mode! 'report-failed)


;;
;; iset-find
;; 查找集合中满足谓词的最小元素。
;;
;; 语法
;; ----
;; (iset-find predicate iset failure)
;;
;; 参数
;; ----
;; predicate : procedure
;; 接受一个整数并返回布尔值的函数。
;;
;; iset : iset
;; 要搜索的集合。
;;
;; failure : procedure
;; 无参函数，当没有元素满足谓词时调用。
;;
;; 返回值
;; -----
;; 返回满足谓词的最小元素，或 failure 的调用结果。
;;
(check (iset-find positive?
         (iset -1 1)
         (lambda () #f)
       ) ;iset-find
  =>
  1
) ;check
(check (iset-find zero?
         (iset -1 1)
         (lambda () #f)
       ) ;iset-find
  =>
  #f
) ;check
(check (iset-find even?
         (iset 1 3 5 7 8 9 10)
         (lambda () #f)
       ) ;iset-find
  =>
  8
) ;check


(check-report)
