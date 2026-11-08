;; make-list-comparator
;; 为列表创建比较器。
;;
;; 语法
;; ----
;; (make-list-comparator element-comparator type-test empty? head tail)
;;
;; 参数
;; ----
;; element-comparator : comparator
;; 用于比较列表元素的比较器。
;; type-test : procedure
;; 列表本身的类型判定过程。
;; empty? : procedure
;; 判断列表是否为空的过程。
;; head : procedure
;; 取列表首元素的过程。
;; tail : procedure
;; 取列表剩余部分的过程。
;; 返回值
;; ----
;; comparator
;; 返回用于比较列表的比较器。
;; 注意
;; ----
;; 按元素顺序逐个比较；较短且为前缀的列表更小。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((num (make-comparator number? = < number-hash))
       (comp (make-list-comparator num list? null? car cdr)))
  (check-true (comparator? comp))
  (check (=? comp '(1 2 3) '(1 2 3)) => #t)
  (check (=? comp '(1 2 3) '(1 2)) => #f)
  (check (<? comp '(1 2) '(1 3)) => #t)
  (check (<? comp '(1 2 3) '(1 2 4)) => #t)
  (check (<? comp '(1) '(1 2)) => #t)
  (check (comparator-test-type comp '(1 2 3)) => #t)
) ;let*

(check-report)
