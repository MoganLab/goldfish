;; make-vector-comparator
;; 为向量创建比较器。
;;
;; 语法
;; ----
;; (make-vector-comparator element-comparator type-test length ref)
;;
;; 参数
;; ----
;; element-comparator : comparator
;; 用于比较向量元素的比较器。
;; type-test : procedure
;; 向量本身的类型判定过程。
;; length : procedure
;; 取向量长度的过程。
;; ref : procedure
;; 取向量元素的过程。
;; 返回值
;; ----
;; comparator
;; 返回用于比较向量的比较器。
;; 注意
;; ----
;; 按元素顺序逐个比较；长度不同则更短的更小。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((num (make-comparator number? = < number-hash))
       (comp (make-vector-comparator num vector? vector-length vector-ref)))
  (check-true (comparator? comp))
  (check (=? comp #(1 2 3) #(1 2 3)) => #t)
  (check (=? comp #(1 2 3) #(1 2)) => #f)
  (check (<? comp #(1 2) #(1 3)) => #t)
  (check (<? comp #(1 2 3) #(1 2 4)) => #t)
  (check (<? comp #(1) #(1 2)) => #t)
  (check (comparator-test-type comp #(1 2 3)) => #t)
) ;let*

(check-report)
