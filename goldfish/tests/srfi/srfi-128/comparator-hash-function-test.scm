;; comparator-hash-function
;; 取出比较器的哈希过程。
;;
;; 语法
;; ----
;; (comparator-hash-function comparator)
;;
;; 参数
;; ----
;; comparator : comparator
;; 返回值
;; ----
;; procedure
;; 返回接受一个对象并返回整数的哈希过程。
;; 注意
;; ----
;; 可用于实现哈希表等需要哈希值的结构。
;; 错误处理
;; ----
;; 无。

(import (liii check) (srfi srfi-128))


(check-set-mode! 'report-failed)


(let* ((comp (make-default-comparator))
       (hash-fn (comparator-hash-function comp)))
  (check-true (procedure? hash-fn))
  (check-true (integer? (hash-fn 1)))
  (check-true (integer? (hash-fn "str")))
) ;let*

;; 相同的值得到相同的哈希
(let* ((comp (make-default-comparator))
       (hash-fn (comparator-hash-function comp)))
  (check (= (hash-fn "abc") (hash-fn (string-copy "abc"))) => #t)
) ;let*

(check-report)
