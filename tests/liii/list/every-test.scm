(import (liii list) (liii check))


(check-set-mode! 'report-failed)


;; every 函数测试
;;
;; 语法
;; ----
;; (every pred clist1 clist2 ...)
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
;; 如果所有元素都满足谓词条件，返回#t，否则返回#f。
;;
;; 示例
;; ----
;; (every integer? '()) => #t
;; (every integer? '(a 3.14 3)) => #f
;; (every integer? '(1 2 3)) => #t


(check (every integer? '()) => #t)
(check (every integer? '(a 3.14 3))
  =>
  #f
) ;check
(check (every integer? '(1 2 3)) => #t)


(check-report)
