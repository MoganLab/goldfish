;; comparator-equality-predicate
;; 取出比较器的相等判定过程。
;;
;; 语法
;; ----
;; (comparator-equality-predicate comparator)
;;
;; 参数
;; ----
;; comparator : comparator
;; 返回值
;; ----
;; procedure
;; 返回接受两个对象并返回布尔值的相等判定过程。
;; 注意
;; ----
;; 可与 =? 配合使用，也可单独调用。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((comp (make-default-comparator))
       (eq-p (comparator-equality-predicate comp)))
  (check-true (procedure? eq-p))
  (check-true (eq-p 1 1))
  (check-false (eq-p 1 2))
) ;let*

;; 自定义相等判定
(let ((comp (make-comparator number? = < number-hash)))
  (check-true ((comparator-equality-predicate comp) 1 1))
  (check-false ((comparator-equality-predicate comp) 1 2))
) ;let

(check-report)
