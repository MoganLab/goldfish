;; comparator-hash
;; 使用比较器计算对象的哈希值。
;;
;; 语法
;; ----
;; (comparator-hash comparator obj)
;;
;; 参数
;; ----
;; comparator : comparator
;; obj : any
;; 待计算哈希值的对象。
;; 返回值
;; ----
;; integer
;; 返回对象的哈希值。
;; 注意
;; ----
;; 相同的对象在使用同一比较器时应得到相同的哈希值。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((comp (make-default-comparator))
       (h (comparator-hash comp "abc")))
  (check-true (integer? h))
  (check (= (comparator-hash comp "abc") h) => #t)
) ;let*

;; 数值比较器的哈希与 number-hash 一致
(let ((comp (make-comparator number? = < number-hash)))
  (check (= (comparator-hash comp 42) (number-hash 42)) => #t)
) ;let

(check-report)
