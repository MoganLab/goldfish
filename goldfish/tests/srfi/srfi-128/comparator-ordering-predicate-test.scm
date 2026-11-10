;; comparator-ordering-predicate
;; 取出比较器的排序判定过程。
;;
;; 语法
;; ----
;; (comparator-ordering-predicate comparator)
;;
;; 参数
;; ----
;; comparator : comparator
;; 返回值
;; ----
;; procedure
;; 返回接受两个对象并返回布尔值的排序判定过程。
;; 注意
;; ----
;; 该过程用于判断前者是否严格小于后者。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((comp (make-default-comparator))
       (lt-p (comparator-ordering-predicate comp)))
  (check-true (procedure? lt-p))
  (check-true (lt-p 1 2))
  (check-false (lt-p 2 1))
  (check-false (lt-p 1 1))
) ;let*

(check-report)
