;; for-each
;; 对列表中的每个元素应用过程，按副作用执行。
;;
;;
;; 语法
;; ----
;; (for-each proc list1 list2 ...)
;;
;; 参数
;; ----
;; proc : procedure?
;; 待应用的过程，参数个数与列表个数相同。
;; list1 list2 ... : list?
;; 输入列表，长度必须相同。
;;
;; 返回值
;; ----
;; 未指定
;; 返回值未指定，依赖副作用。
;;
;; 说明
;; ----
;; 1. 按顺序遍历列表
;; 2. 主要用于副作用操作
;; 3. 多个列表时按并行方式遍历

(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

(let ((result '()))
  (for-each (lambda (x) (set! result (cons x result))) '(1 2 3))
  (check result => '(3 2 1))
) ;let
(let ((result '()))
  (for-each
    (lambda (x y) (set! result (cons (+ x y) result)))
    '(1 2 3)
    '(10 20 30)
  ) ;for-each
  (check result => '(33 22 11))
) ;let

;; R7RS 6.10：多列表参数之一为循环列表时，按其它有限列表的长度遍历
(let ((ls1 (list 10 100 1000))
      (ls2 (list 1 2 3 4 5 6))
      (count 0))
  (set-cdr! (cddr ls1) ls1)
  (for-each (lambda (x y) (set! count (+ count (* x y)))) ls2 ls1)
  (check count => 9750)
) ;let

;; 循环列表在首位时同样按有限列表长度遍历
(let ((ls1 (list 10 100 1000))
      (ls2 (list 1 2 3 4 5 6))
      (count 0))
  (set-cdr! (cddr ls1) ls1)
  (for-each (lambda (x y) (set! count (+ count (* x y)))) ls1 ls2)
  (check count => 9750)
) ;let

;; 循环列表参与时不产生额外迭代：结果与仅有限列表时一致
(let ((ls1 (list 1 2 3))
      (ls2 (list 10 20 30))
      (count 0))
  (set-cdr! (cddr ls1) ls1)
  (for-each (lambda (x y) (set! count (+ count (* x y)))) ls1 ls2)
  (check count => 140)
) ;let

(check-report)
