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
(check (any integer? '(a 3.14 "3")) => #f)
(check (any integer? '(a 3.14 3)) => #t)


;; 点列表：在到达非正规尾部前命中，正常返回
(check (any even? '(1 2 . 3)) => #t)


;; 非列表参数，抛出 wrong-type-arg
(check-catch 'wrong-type-arg (any even? 3))


;; 点列表：遍历到非正规尾部仍未命中，抛出 wrong-type-arg
(check-catch 'wrong-type-arg (any odd? '(2 . 3)))


(check-report)
