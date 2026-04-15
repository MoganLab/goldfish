(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; any 函数测试
;;
;; 语法
;; ----
;; (any pred clist1 clist2 ...)
;;
;; 参数
;; ----
;; pred : procedure?
;; 谓词函数。
;;
;; clist1, clist2, ... : list?
;; 要测试的列表。
;;
;; 返回值
;; ------
;; boolean?
;; 如果至少有一个元素满足谓词条件，返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (any integer? '()) => #f
;; (any integer? '(a 3.14 "3")) => #f
;; (any integer? '(a 3.14 3)) => #t


(check (any integer? '()) => #f)
(check (any integer? '(a 3.14 "3"))
  =>
  #f
) ;check
(check (any integer? '(a 3.14 3)) => #t)


(check-report)
