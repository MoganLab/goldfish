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
(check (every integer? '(a 3.14 3)) => #f)
(check (every integer? '(1 2 3)) => #t)



;; every 遇到首个不满足的元素即短路
(check (every (lambda (x) (< x 2)) '(0 1 2 3)) => #f)

;; 错误用例：非列表参数、点列表
(check-catch 'wrong-type-arg (every integer? 5))
(check-catch 'wrong-type-arg (every (lambda (x) #t) '(1 2 . 3)))

;; 空列表不会调用 pred，pred 不是函数也不报错
(check (every 5 '()) => #t)

;; GC 压力回归测试：闭包 pred + 大量临时 cons，反复触发 GC
(let ((l (iota 10000)))
  (do ((i 0 (+ i 1)))
    ((= i 200))
    (every (lambda (x) (< x 10000)) l)
    (every (lambda (x) (< x 9999)) l)
  ) ;do
  (check (every (lambda (x) (< x 10000)) l) => #t)
  (check (every (lambda (x) (< x 9999)) l) => #f)
) ;let


(check-report)
