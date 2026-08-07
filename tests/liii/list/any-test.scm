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



;; any 命中时将 pred 的真值归一化为 #t
(check (any (lambda (x) (and (> x 2) x)) '(1 2 3)) => #t)
(check (any (lambda (x) (and (> x 2) "hit")) '(1 2 3)) => #t)

;; 错误用例：非列表参数、点列表
(check-catch 'wrong-type-arg (any integer? 5))
(check-catch 'wrong-type-arg (any (lambda (x) #f) '(1 2 . 3)))

;; 空列表不会调用 pred，pred 不是函数也不报错
(check (any 5 '()) => #f)

;; GC 压力回归测试：闭包 pred + 大量临时 cons，反复触发 GC
(let ((l (iota 10000)))
  (do ((i 0 (+ i 1)))
    ((= i 200))
    (any (lambda (x) (= x 9999)) l)
    (any (lambda (x) (= x 10000)) l)
    (any (lambda (x) #f) l)
  ) ;do
  (check (any (lambda (x) (= x 9999)) l) => #t)
  (check (any (lambda (x) (= x 10000)) l) => #f)
  (check (any (lambda (x) #f) l) => #f)
) ;let


(check-report)
