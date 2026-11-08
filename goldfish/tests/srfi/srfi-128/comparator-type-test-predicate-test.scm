;; comparator-type-test-predicate
;; 取出比较器的类型判定过程。
;;
;; 语法
;; ----
;; (comparator-type-test-predicate comparator)
;;
;; 参数
;; ----
;; comparator : comparator
;; 返回值
;; ----
;; procedure
;; 返回接受一个对象并返回布尔值的类型判定过程。
;; 注意
;; ----
;; 可直接用于构造嵌套比较器。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((comp (make-default-comparator))
       (type-test (comparator-type-test-predicate comp)))
  (check-true (procedure? type-test))
  (check-true (type-test 1))
  (check-true (type-test "str"))
) ;let*

;; 自定义类型判定
(let ((comp (make-comparator number? = < number-hash)))
  (check-true ((comparator-type-test-predicate comp) 1))
  (check-false ((comparator-type-test-predicate comp) "str"))
) ;let

(check-report)
